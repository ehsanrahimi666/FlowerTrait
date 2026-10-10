# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
"""Choose validation species and photographs (GBIF, iNaturalist research grade, CC0 / CC BY, original size).

Roles: benchmark (7 flowering observations per species, first photo), pair_sibling (second photo of 2 benchmark
observations), pair_new (2 photos of 1 more flowering observation), negative (1 observation annotated as not
flowering: fruits, buds only or no evidence of flowering)."""
import json
import random
import time

import numpy as np
import pandas as pd
import requests

INAT = "50c9509d-22c7-4a22-a47d-8c48425ef4a7"
REG = {"Europe": ["AT", "DE", "NO", "IT", "ES", "FR", "CH", "GB", "NL", "BE", "DK", "SE", "PL", "CZ"],
       "Middle East": ["IL", "JO", "LB", "CY", "TR"],
       "South America": ["BR", "AR", "UY", "CL", "BO", "PE", "EC", "CO"],
       "Africa": ["ZA", "NA", "LS", "SZ"], "North America": ["US", "CA"], "Oceania": ["AU"],
       "Asia": ["CN", "IN", "NP"]}
TARGET = {"Europe": 15, "Middle East": 10, "South America": 10, "Africa": 10, "North America": 12, "Oceania": 13,
          "Asia": 0}
LIC = {"http://creativecommons.org/publicdomain/zero/1.0/": "CC0_1_0", "http://creativecommons.org/licenses/by/4.0/": "CC_BY_4_0"}
rng = random.Random(20261010)
S = requests.Session()
S.headers["User-Agent"] = "FlowerTrait validation (ehsanrahimi666@gmail.com)"

import os
WORK = os.environ.get("FT_WORK", "work")  # working folder of the validation
PKG = os.environ.get("FT_PKG", ".")  # package source
av = pd.read_csv(f"{WORK}/refl/species_gbif_availability.csv")
ref = pd.read_csv(f"{WORK}/refl/reference_species.csv")
ref["ref_colour"] = ""
import sys
sys.path.insert(0, f"{PKG}/inst/python")
from flowertrait.colour import colour_name  # noqa: E402
ref["ref_colour"] = [str(colour_name(L, a, b)) for L, a, b in zip(ref.L, ref.a, ref.b)]
el = av[(av.ok == True) & (av.n_flowering >= 12) & (av.n_flowering_multi >= 3)].copy()  # noqa: E712
el = el.merge(ref.groupby(["region", "species"]).ref_colour.first().reset_index(), on=["region", "species"], how="left")
el["genus"] = el.gbif_name.str.split(" ").str[0]
chosen = []
for reg, n in TARGET.items():
    d = el[el.region == reg].sort_values("n_flowering", ascending=False)
    if n == 0 or d.empty:
        continue
    by_col = {c: list(g.itertuples()) for c, g in d.groupby("ref_colour")}
    genera, picked = set(), []
    while len(picked) < n and any(by_col.values()):
        for c in sorted(by_col, key=lambda k: -len(by_col[k])):
            while by_col[c]:
                r = by_col[c].pop(0)
                if r.genus not in genera and r.gbif_name not in [p.gbif_name for p in chosen + picked]:
                    genera.add(r.genus)
                    picked.append(r)
                    break
            if len(picked) >= n:
                break
    chosen += picked
    print(reg, len(picked), [(p.gbif_name, p.ref_colour) for p in picked], flush=True)


def records(key, countries, pages=3):
    out = []
    for p in range(pages):
        q = dict(taxonKey=key, datasetKey=INAT, mediaType="StillImage", limit=300, offset=300 * p,
                 license=["CC0_1_0", "CC_BY_4_0"], country=countries)
        for a in range(3):
            try:
                js = S.get("https://api.gbif.org/v1/occurrence/search", params=q, timeout=120).json()
                break
            except Exception:
                time.sleep(3)
        out += js.get("results", [])
        if js.get("endOfRecords", True):
            break
    return out


def photos(r):
    ph = []
    for m in r.get("media", []):
        if m.get("type") != "StillImage" or not m.get("identifier"):
            continue
        lic = LIC.get(m.get("license"), None)
        if lic:
            ph.append((m["identifier"], lic, m.get("rightsHolder") or m.get("creator"), m.get("references")))
    return ph


rows = []
for sp in chosen:
    recs = records(int(sp.taxonKey), REG[sp.region])
    flw = [r for r in recs if "flowers" in (r.get("reproductiveCondition") or "") and photos(r)]
    neg = [r for r in recs if r.get("reproductiveCondition") and "flowers" not in r["reproductiveCondition"] and photos(r)]
    rng.shuffle(flw)
    # prefer distinct observers among benchmark observations
    seen, bench = set(), []
    for r in flw:
        who = r.get("recordedBy")
        if who not in seen:
            seen.add(who)
            bench.append(r)
        if len(bench) == 7:
            break
    for r in flw:
        if len(bench) == 7:
            break
        if r not in bench:
            bench.append(r)
    rest = [r for r in flw if r not in bench and len(photos(r)) >= 2]
    sib = [r for r in bench if len(photos(r)) >= 2][:2]
    new = rest[:1]

    def row(r, k, role):
        u, lic, who, refurl = photos(r)[k]
        return dict(photo_id=f"{r['key']}_{k + 1:02d}", occurrenceKey=str(r["key"]), role=role, region=sp.region,
                    species=sp.gbif_name, ref_species=sp.species, family=r.get("family"), order=r.get("order"),
                    country=r.get("countryCode"), year=r.get("year"), eventDate=r.get("eventDate"),
                    decimalLatitude=r.get("decimalLatitude"), decimalLongitude=r.get("decimalLongitude"),
                    reproductiveCondition=r.get("reproductiveCondition"), recordedBy=r.get("recordedBy"),
                    url=u, license=lic, rightsHolder=who, references=refurl,
                    gbif_url=f"https://www.gbif.org/occurrence/{r['key']}")
    for r in bench:
        rows.append(row(r, 0, "benchmark"))
    for r in sib:
        rows.append(row(r, 1, "pair_sibling"))
    for r in new:
        rows.append(row(r, 0, "pair_new"))
        rows.append(row(r, 1, "pair_new"))
    if neg:
        rng.shuffle(neg)
        rows.append(row(neg[0], 0, "negative"))
    print(sp.region, sp.gbif_name, len(recs), "flowering", len(flw), "neg", len(neg), flush=True)
out = pd.DataFrame(rows)
out.to_csv(f"{WORK}/validation_photos.csv", index=False)
print(out.role.value_counts().to_string())
print(out.groupby("region").species.nunique().to_string())
