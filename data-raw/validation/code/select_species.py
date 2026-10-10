# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
"""Select validation species: reference spectra available and enough research-grade iNaturalist photographs on GBIF
(CC0 / CC BY, annotated as flowering) from the region where the spectra were measured."""
import concurrent.futures as cf
import json
import time

import numpy as np
import pandas as pd
import requests

INAT = "50c9509d-22c7-4a22-a47d-8c48425ef4a7"
REG = {  # region of the reference spectra -> GBIF country codes searched for photographs
    "Europe": ["AT", "DE", "NO", "IT", "ES", "FR", "CH", "GB", "NL", "BE", "DK", "SE", "PL", "CZ"],
    "Middle East": ["IL", "JO", "LB", "CY", "TR"],
    "South America": ["BR", "AR", "UY", "CL", "BO", "PE", "EC", "CO"],
    "Africa": ["ZA", "NA", "LS", "SZ"],
    "North America": ["US", "CA"],
    "Oceania": ["AU"],
    "Asia": ["CN", "IN", "NP"],
}
import os
WORK = os.environ.get("FT_WORK", "work")  # working folder of the validation
PKG = os.environ.get("FT_PKG", ".")  # package source
sp = pd.read_csv(f"{WORK}/refl/reference_species.csv")
sp = sp[~sp.species.str.contains(r" sp$| spp$| x$|hybrid", regex=True)]
cand = sp.drop_duplicates(["region", "species"])[["region", "species"]]
print(len(cand), "candidate region-species", flush=True)


def query(row):
    s = requests.Session()
    s.headers["User-Agent"] = "FlowerTrait validation (ehsanrahimi666@gmail.com)"
    for a in range(3):
        try:
            m = s.get("https://api.gbif.org/v1/species/match", params={"name": row.species}, timeout=60).json()
            if m.get("matchType") in (None, "NONE", "HIGHERRANK") or m.get("rank") not in ("SPECIES",):
                return dict(region=row.region, species=row.species, ok=False, why="no species match")
            if m.get("class") not in ("Magnoliopsida", "Liliopsida"):
                return dict(region=row.region, species=row.species, ok=False, why=f"class {m.get('class')}")
            key = m["usageKey"]
            q = dict(taxonKey=key, datasetKey=INAT, mediaType="StillImage", limit=300,
                     license=["CC0_1_0", "CC_BY_4_0"], country=REG[row.region])
            js = s.get("https://api.gbif.org/v1/occurrence/search", params=q, timeout=90).json()
            res = js.get("results", [])
            fl = [x for x in res if "flowers" in (x.get("reproductiveCondition") or "")]
            multi = [x for x in fl if len([mm for mm in x.get("media", []) if mm.get("type") == "StillImage"]) >= 2]
            return dict(region=row.region, species=row.species, ok=True, why="", taxonKey=key,
                        gbif_name=m.get("canonicalName"), family=m.get("family"), order=m.get("order"),
                        cls=m.get("class"), n_total=js.get("count", 0), n_page=len(res), n_flowering=len(fl),
                        n_flowering_multi=len(multi))
        except Exception as e:
            err = str(e)[:80]
            time.sleep(2)
    return dict(region=row.region, species=row.species, ok=False, why="error " + err)


rows = []
with cf.ThreadPoolExecutor(6) as ex:
    for i, r in enumerate(ex.map(query, cand.itertuples())):
        rows.append(r)
        if i % 100 == 0:
            print(i, flush=True)
out = pd.DataFrame(rows)
out.to_csv(f"{WORK}/refl/species_gbif_availability.csv", index=False)
print(out.groupby("region").apply(lambda d: pd.Series({"n": len(d), "ok": d.ok.sum(),
                                                       ">=12 flowering": (d.n_flowering >= 12).sum(),
                                                       ">=20 flowering": (d.n_flowering >= 20).sum()})).to_string())
