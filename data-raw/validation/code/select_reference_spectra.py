# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
"""Spectra of the validation species (data-raw/validation/reference_spectra.csv).

Keeps the spectra of reference_spectra_full.csv (written by build_reference.py) whose species and region match a
validation species, removes duplicated records (FReD lists some spectra more than once) and rounds reflectance to
five decimals."""
import pandas as pd

import os
WORK = os.environ.get("FT_WORK", "work")  # working folder of the validation
PKG = os.environ.get("FT_PKG", ".")  # package source
m = pd.read_csv(f"{WORK}/validation_photos.csv", dtype=str)
f = pd.read_csv(f"{WORK}/refl/reference_spectra_full.csv", dtype={"sample": str}, low_memory=False)
sel = m[["ref_species", "region"]].drop_duplicates()
x = f.merge(sel, left_on=["species", "region"], right_on=["ref_species", "region"]).drop(columns="ref_species")
x = x.drop_duplicates(["dataset", "species", "sample"]).reset_index(drop=True)
cols = [c for c in x.columns if c.startswith("R") and c[1:].isdigit()]
x[cols] = x[cols].round(5)
x.to_csv(f"{PKG}/data-raw/validation/reference_spectra.csv", index=False)
print(len(x), "spectra of", x.species.nunique(), "species")
