# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
"""Merge the two validation runs into val_all and convert the visual labels from benchmark index to photo_id.

val_all/traits.csv           rows of both runs (one per photo)
val_all/{crops,masks,photos,embeddings}/  symbolic links to the files of both runs
labels.csv                   photo_id, view, state, vis, focus, outline, petals
"""
import os

import pandas as pd

WORK = os.environ.get("FT_WORK", "work")  # working folder of the validation
PKG = os.environ.get("FT_PKG", ".")  # package source
W = WORK
RUNS = [f"{W}/val_run", f"{W}/val_run_b"]
OUT = f"{W}/val_all"
for sub in ("crops", "masks", "photos", "embeddings"):
    os.makedirs(f"{OUT}/{sub}", exist_ok=True)
    for r in RUNS:
        d = f"{r}/{sub}"
        if not os.path.isdir(d):
            continue
        for f in os.listdir(d):
            dst = f"{OUT}/{sub}/{f}"
            if not os.path.lexists(dst):
                os.symlink(f"{d}/{f}", dst)
tr = pd.concat([pd.read_csv(f"{r}/traits.csv", dtype={"photo_id": str}, low_memory=False) for r in RUNS],
               ignore_index=True)
tr = tr.drop_duplicates("photo_id", keep="last")
meta = pd.read_csv(f"{W}/validation_photos.csv", dtype={"photo_id": str, "occurrenceKey": str})
missing = sorted(set(meta.photo_id) - set(tr.photo_id))
tr = tr.set_index("photo_id").loc[[p for p in meta.photo_id if p in set(tr.photo_id)]].reset_index()
tr.to_csv(f"{OUT}/traits.csv", index=False)
print("photos:", len(tr), "missing:", len(missing), missing[:10])

bench = meta[meta.role == "benchmark"].reset_index(drop=True)
lab = pd.read_csv(f"{W}/labels_raw.csv", dtype=str, keep_default_na=False)
lab["idx"] = lab.idx.astype(int)
dup = lab[lab.idx.duplicated(keep=False)]
if len(dup):
    raise SystemExit(f"duplicated labels: {sorted(set(dup.idx))}")
lab["photo_id"] = bench.photo_id.values[lab.idx.values]
lab["petals"] = lab.petals.replace({"": None})
for k, ok in {"view": "FOSBU", "state": "OBWX", "vis": "FPH", "focus": "SB", "outline": "GMCW"}.items():
    bad = lab[~lab[k].isin(list(ok))]
    if len(bad):
        raise SystemExit(f"bad {k} codes: {bad[['idx', k]].values.tolist()}")
lab[["photo_id", "view", "state", "vis", "focus", "outline", "petals"]].to_csv(f"{W}/labels.csv", index=False)
print("labels:", len(lab), "of", len(bench), "benchmark photos; unlabelled idx:",
      sorted(set(range(len(bench))) - set(lab.idx))[:20])
