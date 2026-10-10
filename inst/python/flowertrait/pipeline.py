"""Extended flower traits from a crop and its outline, with a resumable command-line interface.

python -m flowertrait.pipeline --images D:/crops --out traits.csv --masks-dir masks --seg sam
"""
from __future__ import annotations

import argparse
import os
import time
from functools import lru_cache
from pathlib import Path

import cv2
import numpy as np
import pandas as pd

from .colour import colour_metrics, colour_name
from .pattern import pattern_metrics, shape_metrics
from .segment import load_image, quality_metrics, segment
from .shape import radial_metrics

DEFAULTS = dict(max_side=768, seg="grabcut", margin=0.05, cc="none", k=6, merge_de=10.0, min_px=600)
IMG_EXT = {".jpg", ".jpeg", ".png", ".tif", ".tiff", ".webp", ".bmp"}


def extract_traits(img, mask, cfg=None) -> dict:
    """All traits for one crop + flower outline: quality fields, colour, colour names, pattern, radial pattern,
    symmetry and shape."""
    cfg = {**DEFAULTS, **(cfg or {})}
    rec = quality_metrics(img, mask)
    if rec["mask_px"] < cfg["min_px"]:
        rec["status"] = "mask_too_small"
        return rec
    col, aux = colour_metrics(img, mask, cfg["cc"], cfg["k"], cfg["merge_de"])
    rec.update(col)
    rec.update(pattern_metrics(aux["L"], mask, aux["labels"], rec["equiv_diam_px"]))
    rec.update(shape_metrics(mask))
    rad = radial_metrics(aux["lab"], mask)
    rec.update(rad)
    rec["colour_name"] = (str(colour_name(rad["petal_L"], rad["petal_a"], rad["petal_b"]))
                          if np.isfinite(rad["petal_L"]) else "")
    rec["status"] = "ok"
    return rec


@lru_cache(maxsize=1)
def trait_fields() -> tuple:
    """Canonical output columns (from a synthetic five-petal flower), so chunked CSVs always align."""
    img = np.full((200, 200, 3), (60, 110, 50), np.uint8)
    yy, xx = np.mgrid[:200, :200]
    th, rr = np.arctan2(yy - 100, xx - 100), np.hypot(yy - 100, xx - 100)
    mask = rr < 60 + 25 * np.cos(5 * th)
    img[mask] = (230, 200, 40)
    img[rr < 20] = (120, 70, 20)
    return tuple(extract_traits(img, mask).keys()) + ("sec",)


def mask_path(masks_dir, img_path):
    p = Path(img_path)
    return None if masks_dir is None else os.path.join(masks_dir, f"{p.parent.name}__{p.stem}.png")


def process_image(path, cfg=None, mpath=None) -> dict:
    cfg = {**DEFAULTS, **(cfg or {})}
    try:
        img = load_image(path, cfg["max_side"])
        mask = segment(img, cfg["seg"], None, cfg["margin"])
        rec = extract_traits(img, mask, cfg)
        if mpath and mask.any():
            os.makedirs(os.path.dirname(mpath), exist_ok=True)
            cv2.imencode(".png", mask.astype(np.uint8) * 255)[1].tofile(mpath)
        return rec
    except Exception as e:  # keep long batch runs alive; the error is recorded per image
        return {"status": f"error: {type(e).__name__}: {e}"}


def run(table, cfg, out_csv, masks_dir=None, path_col="path", chunk=200) -> pd.DataFrame:
    """Process `table[path_col]`; appends to `out_csv` chunk by chunk and resumes if it exists."""
    cfg = {**DEFAULTS, **cfg}
    done = set()
    if os.path.exists(out_csv):
        done = set(pd.read_csv(out_csv, usecols=[path_col])[path_col].astype(str))
    todo = table[~table[path_col].astype(str).isin(done)].reset_index(drop=True)
    cols = list(table.columns) + [c for c in trait_fields() if c not in table.columns]
    for s in range(0, len(todo), chunk):
        part = todo.iloc[s:s + chunk].reset_index(drop=True)
        res = []
        for p in part[path_col]:
            t0 = time.time()
            rec = process_image(p, cfg, mask_path(masks_dir, p))
            rec["sec"] = round(time.time() - t0, 3)
            res.append(rec)
        block = pd.concat([part, pd.DataFrame(res)], axis=1).reindex(columns=cols)
        block.to_csv(out_csv, mode="a", header=not os.path.exists(out_csv), index=False)
        print(f"{min(s + chunk, len(todo))}/{len(todo)} images processed", flush=True)
    return pd.read_csv(out_csv)


def main(argv=None):
    ap = argparse.ArgumentParser(description="Extract flower colour, pattern and shape traits from crops.")
    src = ap.add_mutually_exclusive_group(required=True)
    src.add_argument("--images", help="folder of images (searched recursively)")
    src.add_argument("--list", help="CSV with one row per image")
    ap.add_argument("--path-col", default="path")
    ap.add_argument("--out", required=True)
    ap.add_argument("--masks-dir")
    ap.add_argument("--seg", default="grabcut", choices=["grabcut", "sam"])
    ap.add_argument("--max-side", type=int, default=768)
    ap.add_argument("--limit", type=int)
    a = ap.parse_args(argv)
    if a.images:
        pc = "path"
        table = pd.DataFrame({pc: sorted(str(p) for p in Path(a.images).rglob("*") if p.suffix.lower() in IMG_EXT)})
    else:
        pc = a.path_col
        table = pd.read_csv(a.list)
    if a.limit:
        table = table.head(a.limit)
    out = run(table, {"seg": a.seg, "max_side": a.max_side}, a.out, a.masks_dir, pc)
    print(out["status"].astype(str).str.slice(0, 40).value_counts().to_string())


if __name__ == "__main__":
    main()
