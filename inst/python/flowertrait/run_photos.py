"""Batch run: flower photos (folder or CSV of URLs/paths) -> flower crop -> outline -> photo filter -> traits.

Validated for flowers of angiosperms photographed in the field (iNaturalist research-grade photographs).

python -m flowertrait.run_photos --input photos/ --out results/
python -m flowertrait.run_photos --input gbif_media.csv --url-col url --id-col photo_id --out results/

Outputs in --out: traits.csv (one row per photo: quality, filter probabilities, keep/reason, core and extended
traits; extended traits whose name is already used by a core or quality column get the suffix _ext),
crops/<id>.jpg (native-resolution crop), masks/<id>.png (outline), photos/ (downloaded originals) and, with
--embeddings, embeddings/<id>.npz (filter features, for re-training the filter heads on new labels).
Resumable: photos already in traits.csv are skipped. Use --no-filter to skip the photo filter (fewer models).
"""
import argparse
import os
import time
import warnings

import cv2
import numpy as np
import pandas as pd

warnings.filterwarnings("ignore")
IMG_EXT = (".jpg", ".jpeg", ".png", ".tif", ".tiff", ".webp", ".bmp")


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--input", required=True, help="folder of photos, or CSV with a URL or path column")
    ap.add_argument("--out", required=True)
    ap.add_argument("--url-col", default="url")
    ap.add_argument("--id-col", default=None, help="CSV column with a unique id (default: file name)")
    ap.add_argument("--prompt", default="flower", help="detector text prompt (validated: 'flower')")
    ap.add_argument("--one-stage", action="store_true", help="faster, less accurate (no re-detection inside the crop)")
    ap.add_argument("--no-filter", action="store_true")
    ap.add_argument("--embeddings", action="store_true", help="save the photo-filter features of each photo")
    ap.add_argument("--white-balance", default="none",
                    choices=["none", "gray_world", "white_patch", "shades_of_gray", "gray_pixels"],
                    help="colour correction of the crop before traits (the photo filter sees the original crop)")
    ap.add_argument("--heads", default=os.path.join(os.path.dirname(__file__), "data", "filter_heads.json"))
    ap.add_argument("--sam-weights", default="sam2.1_b.pt")
    ap.add_argument("--limit", type=int, default=None)
    ap.add_argument("--save-every", type=int, default=10)
    ap.add_argument("--threads", type=int, default=None, help="CPU threads for PyTorch")
    a = ap.parse_args(argv)

    import torch
    if a.threads:
        torch.set_num_threads(a.threads)
    from .colour import estimate_illuminant, linear_to_srgb, srgb_to_linear
    from .photo import Models, load_rgb, prep, process_photo, traits
    from .pipeline import extract_traits
    os.makedirs(a.out, exist_ok=True)
    for d in ("crops", "masks", "photos") + (("embeddings",) if a.embeddings else ()):
        os.makedirs(os.path.join(a.out, d), exist_ok=True)
    if os.path.isdir(a.input):
        items = pd.DataFrame({"path": sorted(os.path.join(a.input, f) for f in os.listdir(a.input)
                                             if f.lower().endswith(IMG_EXT))})
        items["photo_id"] = items.path.map(lambda p: os.path.splitext(os.path.basename(p))[0])
    else:
        items = pd.read_csv(a.input, dtype=str)
        src = a.url_col if a.url_col in items else "path"
        items["path"] = items[src].astype(str)
        items["photo_id"] = (items[a.id_col].astype(str) if a.id_col else
                             items.path.map(lambda p: os.path.splitext(os.path.basename(p.split("?")[0]))[0]))
    if a.limit:
        items = items.head(a.limit)
    out_csv = os.path.join(a.out, "traits.csv")
    done = set(pd.read_csv(out_csv, usecols=["photo_id"], dtype=str).photo_id) if os.path.exists(out_csv) else set()
    items = items[~items.photo_id.isin(done)]
    print(f"{len(items)} photos to process ({len(done)} already done)", flush=True)
    if items.empty:
        return
    M = Models({"prompts": [a.prompt], "sam_weights": a.sam_weights, "two_stage": not a.one_stage})
    F = None
    if not a.no_filter or a.embeddings:
        from .photofilter import PhotoFilter
        F = PhotoFilter(a.heads)
    import requests
    sess = requests.Session()
    sess.headers["User-Agent"] = "FlowerTrait (https://github.com/ehsanrahimi666/FlowerTrait)"
    rows, t0 = [], time.time()
    for n, r in enumerate(items.itertuples(), 1):
        rec = {"photo_id": r.photo_id, "source": r.path}
        t = time.time()
        try:
            p = r.path
            if p.startswith(("http://", "https://")):
                local = os.path.join(a.out, "photos", f"{r.photo_id}.jpg")
                if not os.path.exists(local):
                    x = sess.get(p, timeout=60)
                    x.raise_for_status()
                    with open(local, "wb") as fh:
                        fh.write(x.content)
                p = local
            img = load_rgb(p, M.cfg["photo_max_side"])
            if img is None:
                raise IOError("unreadable image")
            res, crop, mask, _ = process_photo(M, img)
            rec.update(res)
            tcrop = crop
            if a.white_balance != "none":  # traits on the colour-corrected crop; QC and filter on the original
                lin = srgb_to_linear(crop.astype(np.float32) / 255).astype(np.float32)
                e = estimate_illuminant(lin, a.white_balance)
                tcrop = (linear_to_srgb(np.clip(lin / e, 0, 1)) * 255 + 0.5).astype(np.uint8)
                rec.update(traits(prep(tcrop), mask))
            s = 768 / max(crop.shape[:2])
            c768, m768 = ((cv2.resize(tcrop, None, fx=s, fy=s, interpolation=cv2.INTER_AREA),
                           cv2.resize(mask.astype(np.uint8), None, fx=s, fy=s, interpolation=cv2.INTER_NEAREST) > 0)
                          if s < 1 else (tcrop, mask))
            ext = extract_traits(c768, m768) if m768.sum() >= 600 else {}
            if F is not None:
                ext_f = ext if tcrop is crop else None  # the filter heads were trained on uncorrected crops
                feats = F.features(crop, mask, rec, ext=ext_f)
                if a.embeddings:
                    np.savez_compressed(os.path.join(a.out, "embeddings", f"{r.photo_id}.npz"),
                                        **{k.replace("+", "_").replace("(", "_").replace(")", ""): v
                                           for k, v in feats.items()})
                if not a.no_filter:
                    rec.update(F.predict(crop, mask, rec, ext=ext_f))
                    rec.update(F.decide(rec))
            rec.update({(k + "_ext" if k in rec else k): v for k, v in ext.items()})
            cv2.imencode(".jpg", cv2.cvtColor(crop, cv2.COLOR_RGB2BGR), [cv2.IMWRITE_JPEG_QUALITY, 92])[1].tofile(
                os.path.join(a.out, "crops", f"{r.photo_id}.jpg"))
            cv2.imencode(".png", mask.astype(np.uint8) * 255)[1].tofile(os.path.join(a.out, "masks", f"{r.photo_id}.png"))
            rec["error"] = ""
        except Exception as e:
            rec["error"] = f"{type(e).__name__}: {str(e)[:150]}"
        rec["sec"] = round(time.time() - t, 2)
        rows.append(rec)
        print(f"{n}/{len(items)} {r.photo_id}: {rec['sec']} s" + (f" ({rec['error']})" if rec["error"] else ""), flush=True)
        if len(rows) >= a.save_every or n == len(items):
            new = pd.DataFrame(rows)
            if os.path.exists(out_csv):  # align columns with the existing file
                old_cols = pd.read_csv(out_csv, nrows=0).columns.tolist()
                cols = old_cols + [c for c in new.columns if c not in old_cols]
                if cols != old_cols:
                    old = pd.read_csv(out_csv, dtype={"photo_id": str})
                    pd.concat([old, new]).reindex(columns=cols).to_csv(out_csv, index=False)
                else:
                    new.reindex(columns=cols).to_csv(out_csv, mode="a", header=False, index=False)
            else:
                new.to_csv(out_csv, index=False)
            rows = []
            print(f"saved {n}/{len(items)} photos ({(time.time() - t0) / n:.1f} s per photo)", flush=True)


if __name__ == "__main__":
    main()
