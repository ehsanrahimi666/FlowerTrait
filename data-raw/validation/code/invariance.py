# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
"""Trait invariance to simulated camera changes and to resolution, for all traits (core and extended).

For each photo (crop + fixed outline): original traits; one random draw of each change (white balance, exposure,
JPEG re-compression, Gaussian blur, rotation, perspective; defined as in ft_augment()); and copies reduced so that
the flower is 32, 64, 128 and 256 px across. Core traits on the crop (<= 1024 px), extended traits at <= 768 px.
Output: long table (photo, condition, traits) -> ICC in R."""
import os
import sys

import cv2
import numpy as np
import pandas as pd

WORK = os.environ.get("FT_WORK", "work")  # working folder of the validation
PKG = os.environ.get("FT_PKG", ".")  # package source
sys.path.insert(0, f"{PKG}/inst/python")
from flowertrait.colour import linear_to_srgb, srgb_to_linear  # noqa: E402
from flowertrait.photo import prep, traits  # noqa: E402
from flowertrait.pipeline import extract_traits  # noqa: E402

ids = pd.read_csv(sys.argv[1], dtype=str)  # columns photo_id, run
out_csv = sys.argv[2]
rng = np.random.default_rng(1)


def all_traits(img, m, min_px=600):
    rec = dict(traits(prep(img), m, min_px=min_px))
    s = 768 / max(img.shape[:2])
    c, mm = ((cv2.resize(img, None, fx=s, fy=s, interpolation=cv2.INTER_AREA),
              cv2.resize(m.astype(np.uint8), None, fx=s, fy=s, interpolation=cv2.INTER_NEAREST) > 0) if s < 1 else (img, m))
    e = extract_traits(c, mm, {"min_px": min_px}) if mm.sum() >= min_px else {}
    rec.update({(k + "_ext" if k in rec else k): v for k, v in e.items()})
    return rec


def to8(lin):
    return (linear_to_srgb(np.clip(lin, 0, 1)) * 255 + 0.5).astype(np.uint8)


def warp(img, m, H):
    h, w = m.shape
    im = cv2.warpPerspective(img, H, (w, h), flags=cv2.INTER_LINEAR, borderMode=cv2.BORDER_REFLECT)
    mm = cv2.warpPerspective(m.astype(np.uint8), H, (w, h), flags=cv2.INTER_NEAREST, borderMode=cv2.BORDER_CONSTANT,
                             borderValue=0) > 0
    return im, mm


prev = pd.read_csv(out_csv, dtype={"photo_id": str}) if os.path.exists(out_csv) else None
rows = prev.to_dict("records") if prev is not None else []
done = set(prev.photo_id) if prev is not None else set()
for r in ids.itertuples():
    if r.photo_id in done:
        continue
    img = cv2.cvtColor(cv2.imread(os.path.join(r.run, "crops", f"{r.photo_id}.jpg")), cv2.COLOR_BGR2RGB)
    m = cv2.imread(os.path.join(r.run, "masks", f"{r.photo_id}.png"), 0) > 0
    h, w = m.shape
    lin = srgb_to_linear(img.astype(np.float32) / 255).astype(np.float32)
    conds = {"original": (img, m)}
    g = rng.uniform(0.75, 1.25, 2)
    conds["white_balance"] = (to8(lin * np.array([g[0], 1, g[1]], np.float32)), m)
    conds["exposure"] = (to8(lin * rng.uniform(0.6, 1.5)), m)
    q = int(rng.integers(30, 90))
    enc = cv2.imencode(".jpg", cv2.cvtColor(img, cv2.COLOR_RGB2BGR), [cv2.IMWRITE_JPEG_QUALITY, q])[1]
    conds["jpeg"] = (cv2.cvtColor(cv2.imdecode(enc, cv2.IMREAD_COLOR), cv2.COLOR_BGR2RGB), m)
    conds["blur"] = (cv2.GaussianBlur(img, (0, 0), rng.uniform(0.5, 1.5)), m)
    ang = rng.uniform(-30, 30)
    R = np.vstack([cv2.getRotationMatrix2D((w / 2, h / 2), ang, 1.0), [0, 0, 1]])
    conds["rotate"] = warp(img, m, R)
    j = 0.12 * min(w, h)
    src = np.float32([[0, 0], [w, 0], [w, h], [0, h]])
    dst = (src + rng.uniform(-j, j, (4, 2))).astype(np.float32)
    conds["perspective"] = warp(img, m, cv2.getPerspectiveTransform(src, dst))
    D = 2 * np.sqrt(m.sum() / np.pi)
    for rung in (32, 64, 128, 256):
        if rung < D:
            s = rung / D
            nh, nw = max(8, round(h * s)), max(8, round(w * s))
            mm = cv2.resize(m.astype(np.float32), (nw, nh), interpolation=cv2.INTER_AREA) > 0.5
            if mm.sum() >= 50:
                conds[f"D{rung}"] = (cv2.resize(img, (nw, nh), interpolation=cv2.INTER_AREA), mm)
    for cname, (im, mm) in conds.items():
        mp = 50 if cname.startswith("D") else 600
        try:
            t = all_traits(im, mm, mp) if mm.sum() >= mp else {}
        except Exception as e:  # keep going
            t = {"status": f"error {e}"}
        rows.append({"photo_id": r.photo_id, "condition": cname, **t})
    pd.DataFrame(rows).to_csv(out_csv, index=False)
    print(r.photo_id, flush=True)
print("done")
