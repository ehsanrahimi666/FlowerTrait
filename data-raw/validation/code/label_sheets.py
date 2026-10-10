# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
"""Contact sheets of benchmark photos for visual labelling (crop with outline + whole-photo inset).
python label_sheets.py START END  -> sheets of 20 tiles for benchmark photos START..END-1 (processing order)."""
import os
import sys

import pandas as pd

WORK = os.environ.get("FT_WORK", "work")  # working folder of the validation
PKG = os.environ.get("FT_PKG", ".")  # package source
sys.path.insert(0, f"{WORK}/scripts")
from sheet import sheet  # noqa: E402

RUNS = [f"{WORK}/val_run", f"{WORK}/val_run_b"]
OUT = f"{WORK}/label_sheets"
os.makedirs(OUT, exist_ok=True)
meta = pd.read_csv(f"{WORK}/validation_photos.csv", dtype={"photo_id": str, "occurrenceKey": str})
tr = pd.concat([pd.read_csv(os.path.join(r, "traits.csv"), dtype={"photo_id": str}, usecols=["photo_id"])
                for r in RUNS if os.path.exists(os.path.join(r, "traits.csv"))])
where = {p: r for r in RUNS for p in (os.listdir(os.path.join(r, "crops")) if os.path.isdir(os.path.join(r, "crops")) else [])}
bench = meta[(meta.role == "benchmark")].reset_index(drop=True)
bench["idx"] = range(len(bench))
bench = bench[bench.photo_id.isin(tr.photo_id)]
bench[["idx", "photo_id", "species", "region"]].to_csv(os.path.join(OUT, "index.csv"), index=False)
force = len(sys.argv) > 1 and sys.argv[1] == "force"
avail = len(sys.argv) > 1 and sys.argv[1] == "avail"
lab = pd.read_csv(f"{WORK}/labels_raw.csv")
labelled = set(lab.idx)
proc = set(bench.idx)
n_total = int((meta.role == "benchmark").sum())
todo = sorted(i for i in proc if i not in labelled)
blocks = ([todo[k:k + 20] for k in range(0, len(todo) - len(todo) % 20, 20)] if avail else
          [[i for i in range(b0, min(b0 + 20, n_total)) if i not in labelled] for b0 in range(0, n_total, 20)])
for blk in blocks:
    b0 = blk[0] if blk else 0
    if not blk:
        continue
    if not force and not all(i in proc for i in blk):
        continue
    part = bench[bench.idx.isin(blk)]
    out = os.path.join(OUT, f"sheet_{b0:03d}.jpg")
    if os.path.exists(out):
        continue
    items = [dict(crop=os.path.join(where[f"{p}.jpg"], "crops", f"{p}.jpg"),
                  mask=os.path.join(where[f"{p}.jpg"], "masks", f"{p}.png"),
                  photo=os.path.join(where[f"{p}.jpg"], "photos", f"{p}.jpg"), label=f"{i} {sp[:24]}")
             for i, p, sp in zip(part.idx, part.photo_id, part.species) if f"{p}.jpg" in where]
    if items:
        print(sheet(items, out, ncol=5, tile=300), len(items))
print("benchmark processed:", len(bench), "labelled:", len(labelled))
