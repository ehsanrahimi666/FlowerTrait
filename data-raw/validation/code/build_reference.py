# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
"""Compile floral reflectance spectra from open data sets into one species-level reference table.

Output: reference_spectra_long.csv (one row per spectrum, 300-700 nm) and reference_species.csv
(CIELAB under D65 / CIE 1931 2-degree observer of the mean main-colour spectrum per species and data set).
"""
import glob, os, re, zipfile, io
import numpy as np, pandas as pd
import colour  # colour-science

WORK = os.environ.get("FT_WORK", "work")  # working folder of the validation
PKG = os.environ.get("FT_PKG", ".")  # package source
R = f"{WORK}/refl"; F = f"{WORK}/fred"
WL = np.arange(300, 701)
rows = []  # dict(dataset, region, country, species, sample, part, human_colour, symmetry, spec)


def clean_name(s):
    s = re.sub(r"[_.]+", " ", str(s)).strip()
    s = re.sub(r"\s+", " ", s)
    return s


def binom(s):
    p = [re.sub(r"\d+$", "", t) for t in clean_name(s).split(" ")]
    p = [t for t in p if t]
    return " ".join(p[:2]) if len(p) >= 2 else None


def to_grid(w, r):
    w = np.asarray(w, float); r = np.asarray(r, float)
    ok = np.isfinite(w) & np.isfinite(r)
    w, r = w[ok], r[ok]
    if len(w) < 50 or w.min() > 400 or w.max() < 680:
        return None
    o = np.argsort(w); w, r = w[o], r[o]
    out = np.interp(WL, w, r, left=np.nan, right=np.nan)
    return out


def add(dataset, region, country, species, sample, spec, part="", human="", sym=""):
    if spec is None or binom(species) is None:
        return
    rows.append(dict(dataset=dataset, region=region, country=country, species=binom(species), sample=str(sample),
                     part=part, human_colour=human, symmetry=sym, spec=spec))


# ---- Kent Island, New Brunswick, Canada (Dryad 10.5061/dryad.9p8cz8wtm)
d = pd.read_csv(f"{R}/kent_ca/Floral_Reflectance_Kent_Island_Plants.csv")
for c in d.columns[1:]:
    add("Kent Island", "North America", "CA", c, c, to_grid(d.iloc[:, 0], d[c]))
# ---- LeCroy et al., serpentine seeps, California (Dryad 10.5061/dryad.v41ns1rtq)
d = pd.read_csv(f"{R}/lecroy_ca/LeCroy_etal_reflectance_spectra_final.csv", header=None, encoding="latin-1", low_memory=False)
h = d.iloc[29].tolist(); body = d.iloc[30:].reset_index(drop=True)
wcols = [i for i, x in enumerate(h) if re.fullmatch(r"\d+(\.0)?", str(x))]
w = [float(h[i]) for i in wcols]
for _, r in body.iterrows():
    if pd.isna(r[1]):
        continue
    add("LeCroy et al.", "North America", "US", r[1], r[0], to_grid(w, pd.to_numeric(r[wcols], errors="coerce")))
# ---- Namaqualand daisies, South Africa (Dryad 10.5061/dryad.4pn6413): upper petal surfaces
d = pd.read_excel(f"{R}/namaqua_za/Upper and lower petal surfaces.xlsx", sheet_name="Upper petal surfaces")
for c in d.columns[1:]:
    if re.search(r"\bUV\b|cf\b", str(c)):
        continue
    add("Namaqualand daisies", "Africa", "ZA", c, c, to_grid(d.iloc[:, 0], d[c]), part="upper petal")
# ---- Erica, Western Cape, South Africa (Dryad 10.5061/dryad.6djh9w1gz)
d = pd.read_csv(f"{R}/erica_za/Erica.population.spectra.1.csv")
for c in d.columns[1:]:
    m = re.match(r"(?:Erica\.|E\.?)([a-z]+)", c)
    if m:
        add("Erica", "Africa", "ZA", "Erica " + m.group(1), c, to_grid(d.iloc[:, 0], d[c]))
# ---- Australia, Dorin et al. 2024 (Dryad 10.5061/dryad.fn2z34v2c)
for folder in glob.glob(f"{R}/ancient_vision/Flowers *"):
    lst = glob.glob(f"{folder}/_FlowerData FileList*.csv")[0]
    L = pd.read_csv(lst, header=None, skiprows=1, usecols=[0, 2, 3], names=["file", "family", "species"])
    for _, r in L.iterrows():
        f = os.path.join(folder, str(r.file).strip())
        if not os.path.exists(f):
            continue
        s = pd.read_csv(f, header=None)
        add("Dorin et al.", "Oceania", "AU", r.species, r.file, to_grid(s[0], s[1]))
# ---- Dalrymple et al. 2015, Sydney, Australia (Dryad 10.5061/dryad.c47vc)
d = pd.read_excel(f"{R}/dalrymple_au/Dalrymple et al. BJLS. data.xlsx", sheet_name="10 flower spp spectra", header=None)
for j in range(1, d.shape[1]):
    nm = str(d.iloc[0, j])
    add("Dalrymple et al.", "Oceania", "AU", re.sub(r"_\d+$", "", nm), nm, to_grid(d.iloc[1:, 0], d.iloc[1:, j]))
# ---- FReD (Arnold et al. 2010)
fr = pd.read_csv(f"{F}/fred_records.csv", dtype=str).fillna("")
cmap = {"Austria": ("Europe", "AT"), "Germany": ("Europe", "DE"), "Italy": ("Europe", "IT"), "Sardinia": ("Europe", "IT"),
        "Norway": ("Europe", "NO"), "Spain": ("Europe", "ES"), "France": ("Europe", "FR"), "Belgium": ("Europe", "BE"),
        "Israel": ("Middle East", "IL"), "Brazil": ("South America", "BR"), "Argentina": ("South America", "AR"),
        "Bolivia": ("South America", "BO"), "South Africa": ("Africa", "ZA"), "S. Africa": ("Africa", "ZA"),
        "United States of America": ("North America", "US"), "Mexico": ("North America", "MX"),
        "China": ("Asia", "CN"), "Australia": ("Oceania", "AU")}
for _, r in fr.iterrows():
    f = f"{F}/spectra/{r.ID}.csv"
    if r.Country not in cmap or not os.path.exists(f):
        continue
    sec = r["Flower Section"].lower()
    if re.search(r"leaf|gland|anther|stamen|stigma|pollen|catkin|stem|fruit|seed|bract|sepal|calyx", sec):
        continue
    main = r["Main Colour"]
    if main == "N":
        continue
    s = pd.read_csv(f, header=None)
    sym = "radial" if ("radially" in sec or "bell" in sec) else ("bilateral" if "zygomorph" in sec else "")
    reg, cc = cmap[r.Country]
    add("FReD", reg, cc, f"{r.Genus} {r.Species}", r.ID, to_grid(s[1], s[2]), part=sec, human=r["Human Colour"].lower(), sym=sym)

# ---- spectra -> CIELAB (D65, CIE 1931 2 deg); reflectance in % is rescaled to 0-1; >700 nm held at the 700 nm value
cmfs = colour.MSDS_CMFS["CIE 1931 2 Degree Standard Observer"]
ill = colour.SDS_ILLUMINANTS["D65"]
shape = colour.SpectralShape(360, 780, 1)
cm = cmfs.copy().align(shape); il = ill.copy().align(shape)
wl_full = np.arange(360, 781)
xbar, ybar, zbar = cm.values.T
E = il.values
k = 100 / np.sum(E * ybar)
WP = colour.CCS_ILLUMINANTS["CIE 1931 2 Degree Standard Observer"]["D65"]


def lab_of(spec):
    s = spec.copy()
    if np.nanmax(s) > 1.5:
        s = s / 100.0
    s = np.clip(np.nan_to_num(s, nan=0.0), 0, 1.2)
    full = np.interp(wl_full, WL, s)  # constant extrapolation below 300 / above 700 nm
    XYZ = k * np.array([np.sum(E * full * xbar), np.sum(E * full * ybar), np.sum(E * full * zbar)])
    return colour.XYZ_to_Lab(XYZ / 100, WP)


long = []
for r in rows:
    if np.isnan(r["spec"]).mean() > 0.2:
        continue
    L, a, b = lab_of(r["spec"])
    long.append({**{k2: v for k2, v in r.items() if k2 != "spec"}, "L": L, "a": a, "b": b,
                 **{f"R{w}": v for w, v in zip(WL[::10], r["spec"][::10])}})
long = pd.DataFrame(long)
long.to_csv(f"{WORK}/refl/reference_spectra_long.csv", index=False)
wide = pd.DataFrame([{**{k2: v for k2, v in r.items() if k2 != "spec"}, **{f"R{w}": v for w, v in zip(WL, r["spec"])}}
                     for r in rows if np.isnan(r["spec"]).mean() <= 0.2])
wide.to_csv(f"{WORK}/refl/reference_spectra_full.csv", index=False)
# species level: mean Lab of the spectra (per data set; FReD: only main-colour petal/whole-flower records)
g = long.groupby(["dataset", "region", "country", "species"])
sp = g.agg(n_spectra=("L", "size"), L=("L", "mean"), a=("a", "mean"), b=("b", "mean"),
           human_colour=("human_colour", lambda x: ";".join(sorted(set([v for v in x if v])))),
           symmetry=("symmetry", lambda x: (pd.Series([v for v in x if v]).mode().iloc[0] if any(x) else ""))).reset_index()
sp["C"] = np.hypot(sp.a, sp.b); sp["h"] = np.degrees(np.arctan2(sp.b, sp.a)) % 360
sp.to_csv(f"{WORK}/refl/reference_species.csv", index=False)
print(long.groupby(["dataset", "region"]).species.nunique().to_string())
print("species total:", sp.species.nunique(), "rows:", len(sp))
