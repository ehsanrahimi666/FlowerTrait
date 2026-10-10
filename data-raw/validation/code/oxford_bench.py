# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
"""Benchmark detectors and segmenters against manual flower outlines (Oxford 17 Flowers trimaps).

Ground truth: trimap colours (RGB) (128,0,0) = flower; (0,0,0) = unknown boundary band (ignored); all other
colours = background. Each automatic outline is compared with the ground-truth flower component that it overlaps
most (8-connected components of the flower label), with unknown pixels excluded:
IoU = |A & G| / |A | G|, precision = |A & F| / |A| (F = all flower pixels), recall = |A & G| / |G|.
"""
import glob
import json
import os
import sys
import time

import cv2
import numpy as np
import pandas as pd
import torch

WORK = os.environ.get("FT_WORK", "work")  # working folder of the validation
PKG = os.environ.get("FT_PKG", ".")  # package source
sys.path.insert(0, f"{PKG}/inst/python")
torch.set_num_threads(int(os.environ.get("THREADS", 2)))
from flowertrait.photo import DEFAULTS, clean, detect, localize, pick_flower, scale_box, square_crop  # noqa: E402
from flowertrait.segment import grabcut_mask  # noqa: E402

OX = f"{WORK}/oxford"
OUT = f"{WORK}/oxford_bench"
os.makedirs(OUT, exist_ok=True)
N_PER_CLASS = int(os.environ.get("N_PER_CLASS", 6))
CLASSES = ["Daffodil", "Snowdrop", "Lily Valley", "Bluebell", "Crocus", "Iris", "Tigerlily", "Tulip", "Fritillary",
           "Sunflower", "Daisy", "Colts' Foot", "Dandelion", "Cowslip", "Buttercup", "Windflower", "Pansy"]


def gt(path):
    t = cv2.imread(path)[:, :, ::-1]
    fg = (t == [128, 0, 0]).all(-1)
    unk = (t == [0, 0, 0]).all(-1)
    return fg, unk


def score(m, fg, unk):
    valid = ~unk
    a = m & valid
    if a.sum() == 0:
        return dict(iou=0.0, precision=np.nan, recall=0.0, n_gt=0)
    n, lab = cv2.connectedComponents(fg.astype(np.uint8), connectivity=8)
    big = [k for k in range(1, n) if (lab == k).sum() >= 0.005 * fg.size]
    ov = [(np.logical_and(a, lab == k).sum(), k) for k in range(1, n)]
    k = max(ov)[1] if ov and max(ov)[0] > 0 else None
    if k is None:
        return dict(iou=0.0, precision=float((a & fg).sum() / a.sum()), recall=0.0, n_gt=len(big))
    g = (lab == k) & valid
    return dict(iou=float((a & g).sum() / (a | g).sum()), precision=float((a & fg).sum() / a.sum()),
                recall=float((a & g).sum() / g.sum()), n_gt=len(big))


class Det:
    def __init__(self, name):
        from transformers import AutoModelForZeroShotObjectDetection, AutoProcessor
        self.name = name
        self.cfg = dict(DEFAULTS)
        hub = {"gdino_b": "IDEA-Research/grounding-dino-base", "gdino_t": "IDEA-Research/grounding-dino-tiny",
               "owlv2_b": "google/owlv2-base-patch16-ensemble"}[name]
        self.proc = AutoProcessor.from_pretrained(hub)
        self.det = AutoModelForZeroShotObjectDetection.from_pretrained(hub).eval()
        self.device = "cpu"

    def __call__(self, img, size=None):
        if self.name.startswith("gdino"):
            return detect(self, [img], size)[0]
        from PIL import Image
        inp = self.proc(text=[["a photo of a flower"]], images=Image.fromarray(img), return_tensors="pt")
        with torch.no_grad():
            out = self.det(**inp)
        h, w = img.shape[:2]
        s = max(h, w)  # OWLv2 pads to a square
        res = self.proc.post_process_grounded_object_detection(out, threshold=0.05, target_sizes=torch.tensor([[s, s]]))[0]
        b = res["boxes"].numpy().reshape(-1, 4)
        b[:, [0, 2]] = b[:, [0, 2]].clip(0, w)
        b[:, [1, 3]] = b[:, [1, 3]].clip(0, h)
        return b, res["scores"].numpy().reshape(-1)


def stage1(det, img):
    """Stage-1 detection on the whole photograph (shared by the one- and two-stage variants)."""
    cfg = det.cfg
    H, W = img.shape[:2]
    t0 = time.time()
    b1, s1 = det(img, cfg["stage1_size"])
    box1, sc1, _, _ = localize(b1, s1, W, H, cfg)
    if box1 is None:
        box1, sc1 = [0.03 * W, 0.03 * H, 0.97 * W, 0.97 * H], 0.0
    return box1, sc1, time.time() - t0


def boxes(det, img, s1, two_stage=True):
    """Square crop around the stage-1 box and, for two stages, a stage-2 box inside the crop (the FlowerTrait rule)."""
    cfg = det.cfg
    box1, sc1, t1 = s1
    crop, (x0, y0, x1, y1), s = square_crop(img, box1, cfg)
    h, w = crop.shape[:2]
    t0 = time.time()
    if two_stage:
        b2, s2 = det(crop, cfg["stage2_size"])
        box, sc = pick_flower(b2, s2, w, h, cfg, central=True)
        box = box or [0.03 * w, 0.03 * h, 0.97 * w, 0.97 * h]
        sc = sc or 0.0
        t2 = time.time() - t0
    else:
        box = [max(0.0, (box1[0] - x0) * s), max(0.0, (box1[1] - y0) * s), min(float(w), (box1[2] - x0) * s),
               min(float(h), (box1[3] - y0) * s)]
        sc, t2 = sc1, 0.0
    return crop, (x0, y0, x1, y1, s), box, sc, t1 + t2


def to_full(mc, geo, shape):
    x0, y0, x1, y1, s = geo
    m = cv2.resize(mc.astype(np.uint8), (x1 - x0, y1 - y0), interpolation=cv2.INTER_NEAREST) > 0
    full = np.zeros(shape, bool)
    full[y0:y1, x0:x1] = m
    return full


def main():
    from ultralytics import SAM
    tri = sorted(glob.glob(f"{OX}/trimaps/image_*.png"))
    rng = np.random.default_rng(17)
    sel = []
    for c in range(17):
        cl = [t for t in tri if (int(os.path.basename(t)[6:10]) - 1) // 80 == c]
        sel += list(rng.choice(cl, min(N_PER_CLASS, len(cl)), replace=False))
    # process round-robin over classes, so that a run stopped early stays balanced across classes
    by_cls = {}
    for t in sel:
        by_cls.setdefault((int(os.path.basename(t)[6:10]) - 1) // 80, []).append(t)
    sel = [by_cls[c][i] for i in range(N_PER_CLASS) for c in sorted(by_cls) if i < len(by_cls[c])]
    print(len(sel), "images", flush=True)
    sams = {k: SAM(f"{WORK}/models/{k}.pt") for k in ["sam2.1_b", "sam2.1_s", "sam2.1_t"]}
    dets = {k: Det(k) for k in ["gdino_b", "owlv2_b"]}
    shard, n_shard = (int(x) for x in os.environ.get("SHARD", "0/1").split("/"))
    out_csv = f"{OUT}/results.csv" if n_shard == 1 else f"{OUT}/results_shard{shard}.csv"
    done = set()
    for f in glob.glob(f"{OUT}/results*.csv"):
        done |= set(pd.read_csv(f).image)
    sel = [t for k, t in enumerate(sel) if k % n_shard == shard]
    for i, t in enumerate(sel):
        name = os.path.basename(t).replace(".png", "")
        if name in done:
            continue
        img = cv2.imread(f"{OX}/jpg/{name}.jpg")[:, :, ::-1].copy()
        fg, unk = gt(t)
        H, W = img.shape[:2]
        rows = []
        variants = [("gdino_b", True), ("gdino_b", False), ("owlv2_b", True), ("none", False)]
        s1 = {k: stage1(dets[k], img) for k in dets}
        for dname, two in variants:
            if dname == "none":
                crop, geo, box, sc, td = img, (0, 0, W, H, 1.0), [0.03 * W, 0.03 * H, 0.97 * W, 0.97 * H], 0.0, 0.0
            else:
                crop, geo, box, sc, td = boxes(dets[dname], img, s1[dname], two)
            segs = ["sam2.1_b"] + (["sam2.1_s", "sam2.1_t", "grabcut"] if (dname == "gdino_b" and two) else
                                   ["grabcut"] if dname == "none" else [])
            for sname in segs:
                t1 = time.time()
                if sname == "grabcut":
                    mc = grabcut_mask(crop, box)
                    mc = clean(mc, box)
                    stab = np.nan
                else:
                    h, w = crop.shape[:2]
                    pr = [box] + [scale_box(box, f, w, h) for f in (0.9, 1.1)]
                    r = sams[sname](cv2.cvtColor(crop, cv2.COLOR_RGB2BGR), bboxes=pr, verbose=False)[0]
                    ms = [np.zeros((h, w), bool) for _ in pr]
                    if r.masks is not None:
                        for d, c in zip(r.masks.data.cpu().numpy(), r.boxes.cls.cpu().numpy().astype(int)):
                            ms[c] = cv2.resize(d.astype(np.uint8), (w, h), interpolation=cv2.INTER_NEAREST) > 0
                    ms = [clean(m, b) for m, b in zip(ms, pr)]
                    mc = ms[0]
                    u = [(mc | x).sum() for x in ms[1:]]
                    stab = min(float((mc & x).sum() / uu) if uu else 0.0 for x, uu in zip(ms[1:], u))
                ts = time.time() - t1
                full = to_full(mc, geo, (H, W))
                bt = float(np.concatenate([mc[0], mc[-1], mc[:, 0], mc[:, -1]]).mean()) if mc.any() else np.nan
                rec = dict(image=name, cls=CLASSES[(int(name[6:10]) - 1) // 80], detector=dname, two_stage=two,
                           segmenter=sname, det_score=sc, stab=stab, px=int(mc.sum()),
                           mask_ok=bool(sc > 0 and mc.sum() >= 600 and bt <= 0.10) if dname != "none" else bool(mc.sum() >= 600),
                           sec_detect=td, sec_segment=ts, **score(full, fg, unk))
                rows.append(rec)
                if dname == "gdino_b" and two and sname == "sam2.1_b":
                    cv2.imwrite(f"{OUT}/{name}_mask.png", full.astype(np.uint8) * 255)
        pd.DataFrame(rows).to_csv(out_csv, mode="a", header=not os.path.exists(out_csv), index=False)
        print(i, name, f"{sum(r['sec_detect'] + r['sec_segment'] for r in rows):.1f}s",
              [round(r["iou"], 3) for r in rows], flush=True)


if __name__ == "__main__":
    main()
