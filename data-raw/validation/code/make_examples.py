# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
"""Example photographs and gallery thumbnails shipped with FlowerTrait.

1. Example photos (inst/extdata): the chosen validation photos reduced to 800 px (longer side) and processed with the
   final back end (filter heads included) -> photos/, crops/, masks/, example_results.csv, ATTRIBUTION.csv.
2. Gallery (inst/extdata/gallery): crops and outlines of validation photos reduced to 256 px, used by the figure script.

python make_examples.py examples|gallery
"""
import os
import subprocess
import sys

import cv2
import numpy as np
import pandas as pd

WORK = os.environ.get("FT_WORK", "work")  # working folder of the validation
PKG = os.environ.get("FT_PKG", ".")  # package source
W = WORK
EXT = f"{PKG}/inst/extdata"
meta = pd.read_csv(f"{W}/validation_photos.csv", dtype={"photo_id": str, "occurrenceKey": str})
lic = {"CC_BY_4_0": "http://creativecommons.org/licenses/by/4.0/", "CC0_1_0": "http://creativecommons.org/publicdomain/zero/1.0/"}


def attribution(ids, extra=None):
    m = meta.set_index("photo_id").loc[ids].reset_index()
    out = pd.DataFrame(dict(photo_id=m.photo_id, species=m.species, family=m.family, order=m.order, region=m.region,
                            country=m.country, occurrenceKey=m.occurrenceKey, gbif_url=m.gbif_url, image_url=m.url,
                            rights_holder=m.rightsHolder, license=m.license.map(lic), year=m.year))
    if extra is not None:
        out = out.merge(extra, on="photo_id", how="left")
    return out


def examples(ids, view_label):
    for sub in ("photos", "crops", "masks"):
        os.makedirs(f"{EXT}/{sub}", exist_ok=True)
    src = f"{W}/examples_src"
    os.makedirs(src, exist_ok=True)
    rows = []
    for p in ids:
        img = cv2.imread(f"{W}/val_all/photos/{p}.jpg")
        s = 800 / max(img.shape[:2])
        if s < 1:
            img = cv2.resize(img, None, fx=s, fy=s, interpolation=cv2.INTER_AREA)
        cv2.imwrite(f"{EXT}/photos/{p}.jpg", img, [cv2.IMWRITE_JPEG_QUALITY, 90])
        rows.append(dict(photo_id=p, path=f"{EXT}/photos/{p}.jpg"))
    pd.DataFrame(rows).to_csv(f"{src}/examples.csv", index=False)
    out = f"{W}/examples_run"
    cmd = [sys.executable, "-m", "flowertrait.run_photos", "--input", f"{src}/examples.csv", "--url-col", "path",
           "--id-col", "photo_id", "--out", out, "--sam-weights", f"{W}/models/sam2.1_b.pt", "--save-every", "1"]
    env = dict(os.environ, PYTHONPATH=f"{PKG}/inst/python")
    subprocess.run(cmd, check=True, env=env)
    res = pd.read_csv(f"{out}/traits.csv", dtype={"photo_id": str})
    res["source"] = "photos/" + res.photo_id + ".jpg"
    res = res[["photo_id", "source"] + [c for c in res.columns if c not in ("photo_id", "source")]]
    res.to_csv(f"{EXT}/example_results.csv", index=False)
    for p in ids:
        for sub, ext in (("crops", "jpg"), ("masks", "png")):
            os.replace(f"{out}/{sub}/{p}.{ext}", f"{EXT}/{sub}/{p}.{ext}") if os.path.exists(f"{out}/{sub}/{p}.{ext}") else None
    att = attribution(ids, pd.DataFrame(dict(photo_id=ids, view_label=view_label)))
    att["modification"] = "downscaled to 800 px (longest side) and re-encoded as JPEG"
    att.to_csv(f"{EXT}/ATTRIBUTION.csv", index=False)
    print(res[["photo_id", "mask_ok", "p_usable", "p_face_view", "colour_name", "petal_n"]].to_string())


def gallery(ids, note):
    g = f"{EXT}/gallery"
    os.makedirs(g, exist_ok=True)
    for p in ids:
        c = cv2.imread(f"{W}/val_all/crops/{p}.jpg")
        m = cv2.imread(f"{W}/val_all/masks/{p}.png", cv2.IMREAD_GRAYSCALE)
        s = 256 / max(c.shape[:2])
        c = cv2.resize(c, None, fx=s, fy=s, interpolation=cv2.INTER_AREA)
        m = (cv2.resize((m > 0).astype(np.uint8) * 255, (c.shape[1], c.shape[0]), interpolation=cv2.INTER_AREA) >= 128)
        cv2.imwrite(f"{g}/{p}.jpg", c, [cv2.IMWRITE_JPEG_QUALITY, 88])
        cv2.imwrite(f"{g}/{p}.png", m.astype(np.uint8) * 255)
    att = attribution(ids, pd.DataFrame(dict(photo_id=ids, note=note)))
    att["modification"] = "crop and outline of the FlowerTrait pipeline, reduced to 256 px"
    att.to_csv(f"{g}/ATTRIBUTION.csv", index=False)
    print(len(ids), "gallery images")


if __name__ == "__main__":
    sel = pd.read_csv(f"{W}/{sys.argv[1]}_selection.csv", dtype=str, keep_default_na=False)
    if sys.argv[1] == "examples":
        examples(list(sel.photo_id), list(sel.view_label))
    else:
        gallery(list(sel.photo_id), list(sel.note))
