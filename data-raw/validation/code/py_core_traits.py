# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
"""Core traits computed by the Python back end from saved crops (JPEG) and outlines (PNG), i.e. from the same
inputs that ft_traits() reads in R. Used for the R-Python agreement check and the test fixture.

python py_core_traits.py FOLDER_WITH_crops_AND_masks OUT.csv [ID_LIST.txt]
"""
import os
import sys

import cv2
import numpy as np
import pandas as pd

WORK = os.environ.get("FT_WORK", "work")  # working folder of the validation
PKG = os.environ.get("FT_PKG", ".")  # package source
sys.path.insert(0, f"{PKG}/inst/python")
from flowertrait.photo import prep, quality, traits  # noqa: E402

root, out = sys.argv[1], sys.argv[2]
ids = ([x.strip() for x in open(sys.argv[3]) if x.strip()] if len(sys.argv) > 3 else
       sorted(f[:-4] for f in os.listdir(os.path.join(root, "crops")) if f.endswith(".jpg")))
rows = []
for p in ids:
    c = cv2.cvtColor(cv2.imread(os.path.join(root, "crops", f"{p}.jpg")), cv2.COLOR_BGR2RGB)
    m = cv2.imread(os.path.join(root, "masks", f"{p}.png"), cv2.IMREAD_GRAYSCALE) > 0
    bt = float(np.concatenate([m[0], m[-1], m[:, 0], m[:, -1]]).mean()) if m.any() else np.nan
    rec = dict(photo_id=p, px=int(m.sum()), border_touch=bt)
    rec.update(quality(c, m))
    rec.update(traits(prep(c), m))
    rows.append(rec)
pd.DataFrame(rows).to_csv(out, index=False)
print(len(rows), "photos ->", out)
