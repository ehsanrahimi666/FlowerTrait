# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
"""Train and evaluate the photo-filter heads from visual labels of the benchmark photos.

Labels (labels.csv): photo_id, view (F face, O oblique, S side, B back/below, U unclear), state (O open, B bud,
W wilted, X no flower), vis (F whole flower, P partly hidden or cut off, H mostly hidden), focus (S sharp, B blurred),
outline (G good, M minor error, C several flowers outlined together, W wrong), petals (visible petals, lobes or rays; NA if not countable).
"""
import json
import os
import sys

import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression, LogisticRegressionCV
from sklearn.metrics import balanced_accuracy_score, roc_auc_score
from sklearn.model_selection import RepeatedStratifiedKFold
from sklearn.preprocessing import StandardScaler

WORK = os.environ.get("FT_WORK", "work")  # working folder of the validation
PKG = os.environ.get("FT_PKG", ".")  # package source
RUN = f"{WORK}/val_all"
SPEC = f"{PKG}/inst/python/flowertrait/data/filter_heads.json"
OUT = f"{WORK}/filter"
os.makedirs(OUT, exist_ok=True)
spec = json.load(open(SPEC))
SHAPE_KEYS = ("sym_bilateral", "sym_rotational", "lobe_depth", "radius_cv", "fill_circle", "aspect", "solidity_shape",
              "shape_circularity", "shape_roughness")
CS = np.logspace(-4, 2, 13)

tr = pd.read_csv(os.path.join(RUN, "traits.csv"), dtype={"photo_id": str}, low_memory=False)
lab = pd.read_csv(f"{WORK}/labels.csv", dtype={"photo_id": str})
meta = pd.read_csv(f"{WORK}/validation_photos.csv", dtype={"photo_id": str})


def qc_frame(t, zs):
    q = pd.DataFrame(index=t.index)
    for k in spec["qc_features"]:
        if k.startswith("zs_"):
            continue
        if k in SHAPE_KEYS:
            q[k] = t[k + "_ext"] if k + "_ext" in t else t.get(k)
        elif k == "log_sharp":
            q[k] = np.log1p(t["sharp"])
        elif k == "log_px":
            q[k] = np.log(np.maximum(1.0, t["flower_px_native"].fillna(1.0)))
        else:
            q[k] = t[k].astype(float) if k in t else np.nan
    for j, k in enumerate(spec["prompts"]):
        q["zs_" + k] = zs[:, j]
    return q[spec["qc_features"]]


def load_emb(ids):
    keys = ["clip", "dino", "clip_dino", "clip_dino_masked", "zeroshot"]
    out = {k: [] for k in keys}
    for p in ids:
        z = np.load(os.path.join(RUN, "embeddings", f"{p}.npz"))
        for k in keys:
            out[k].append(z[k])
    return {k: np.vstack(v) for k, v in out.items()}


ok = tr[tr.photo_id.isin([f[:-4] for f in os.listdir(os.path.join(RUN, "embeddings"))])].reset_index(drop=True)
E = load_emb(ok.photo_id)
Q = qc_frame(ok, E["zeroshot"])
med = Q.median().fillna(0.0)
Qf = Q.fillna(med).values
SETS = {"QC+shape": Qf, "clip": E["clip"], "dino": E["dino"], "clip+dino": E["clip_dino"],
        "clip+dino(masked)": E["clip_dino_masked"], "clip+dino+QC": np.hstack([E["clip"], E["dino"], Qf])}
P = list(spec["prompts"])
zs = pd.DataFrame(E["zeroshot"], columns=P)
ZERO = {"face_view": zs["face"] - zs[["side", "back"]].max(1),
        "open_flower": zs["open"] - zs[["bud", "wilted", "fruit", "leaves"]].max(1),
        "fully_visible": zs["full"] - zs["cut"], "sharp": zs["sharp"] - zs["blurry"]}
ZERO["usable"] = sum(ZERO[k] for k in ["open_flower", "fully_visible", "sharp"]) / 3

L = ok[["photo_id"]].merge(lab, on="photo_id", how="left")
labelled = L.view.notna().values
y_all = {
    "usable": ((L.state == "O") & (L.vis == "F") & (L.focus == "S") & (L.view != "U")).astype(float),
    "face_view": (L.view == "F").astype(float).where(L.view != "U"),
    "open_flower": (L.state == "O").astype(float),
    "fully_visible": (L.vis == "F").astype(float),
    "sharp": (L.focus == "S").astype(float),
}
# second photographs of benchmark observations get predictions from the fold in which their sibling was held out
occ = ok.photo_id.map(dict(zip(meta.photo_id, meta.occurrenceKey))).values
rows, oof, heads = [], {}, {}
for qn, y in y_all.items():
    use = labelled & y.notna().values
    idx_use = np.where(use)[0]
    yy = y[use].values.astype(int)
    best = None
    for sname, X in SETS.items():
        Xu = X[use]
        aucs, bas, pred = [], [], np.zeros((len(yy), 2))
        sib = np.full((len(ok), 2), np.nan)
        rk = RepeatedStratifiedKFold(n_splits=5, n_repeats=2, random_state=1)
        for f, (a, b) in enumerate(rk.split(Xu, yy)):
            sc = StandardScaler().fit(Xu[a])
            m = LogisticRegressionCV(Cs=CS, cv=3, scoring="roc_auc", max_iter=5000, n_jobs=2).fit(sc.transform(Xu[a]), yy[a])
            pred[b, f // 5] = m.predict_proba(sc.transform(Xu[b]))[:, 1]
            srows = np.where(np.isin(occ, occ[idx_use[b]]) & ~labelled)[0]
            if len(srows):
                sib[srows, f // 5] = m.predict_proba(sc.transform(X[srows]))[:, 1]
        for r in range(2):
            aucs.append(roc_auc_score(yy, pred[:, r]))
            bas.append(balanced_accuracy_score(yy, pred[:, r] >= 0.5))
        rows.append(dict(question=qn, features=sname, n=len(yy), positives=int(yy.sum()), auc=np.mean(aucs),
                         auc_sd=np.std(aucs), balanced_accuracy=np.mean(bas)))
        if best is None or np.mean(aucs) > best[0]:
            best = (np.mean(aucs), sname, pred.mean(1), np.nanmean(sib, 1))
        print(qn, sname, round(np.mean(aucs), 3), flush=True)
    z = ZERO[qn].values[use]
    rows.append(dict(question=qn, features="zero-shot CLIP", n=len(yy), positives=int(yy.sum()), auc=roc_auc_score(yy, z),
                     auc_sd=0.0, balanced_accuracy=np.nan))
    sname = best[1]
    oof[qn] = pd.Series(best[3], index=L.index)
    oof[qn][use] = best[2]
    # final head on all labelled photos, penalty chosen by cross-validation
    X = SETS[sname][use]
    sc = StandardScaler().fit(X)
    mcv = LogisticRegressionCV(Cs=CS, cv=5, scoring="roc_auc", max_iter=5000, n_jobs=2).fit(sc.transform(X), yy)
    C = float(mcv.C_[0])
    m = LogisticRegression(C=C, max_iter=5000).fit(sc.transform(X), yy)
    heads[qn] = dict(features=sname, mean=sc.mean_.tolist(), scale=sc.scale_.tolist(), coef=m.coef_[0].tolist(),
                     intercept=float(m.intercept_[0]), C=C)
res = pd.DataFrame(rows)
res["used"] = False
for qn, h in heads.items():
    res.loc[(res.question == qn) & (res.features == h["features"]), "used"] = True
res.to_csv(os.path.join(OUT, "filter_eval.csv"), index=False)
# out-of-fold probabilities for labelled photos and their siblings; heads applied to all other photos
pr = pd.DataFrame({"photo_id": L.photo_id})
for qn, h in heads.items():
    X = SETS[h["features"]]
    full = 1 / (1 + np.exp(-(((X - np.array(h["mean"])) / np.array(h["scale"])) @ np.array(h["coef"]) + h["intercept"])))
    v = oof[qn].values.copy()
    v[np.isnan(v)] = full[np.isnan(v)]
    pr["p_" + qn] = v
    pr["p_" + qn + "_full"] = full
pr.to_csv(os.path.join(OUT, "filter_preds.csv"), index=False)
spec2 = dict(spec)
spec2["qc_medians"] = [float(med[k]) for k in spec["qc_features"]]
spec2["heads"] = heads
spec2["trained_on"] = (f"{int(labelled.sum())} visually labelled iNaturalist photographs of flowers of "
                       f"{ok[labelled].photo_id.map(dict(zip(meta.photo_id, meta.species))).nunique()} species "
                       "(FlowerTrait validation, October 2026)")
json.dump(spec2, open(os.path.join(OUT, "filter_heads.json"), "w"))
print(res[res.used | (res.features == "zero-shot CLIP") | (res.features == "QC+shape")].to_string())
