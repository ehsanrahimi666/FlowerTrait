"""Photo -> traits pipeline for flowers (back end of the FlowerTrait R package).

Stage 1  Grounding DINO-base ("flower") on the full photo -> main flower box, other flowers, cut-by-frame flag.
         Main flower: the largest box among those scoring at least 85% of the best box, unless that box encloses
         two or more other confident flowers (an inflorescence), in which case the next largest is taken.
Crop     square crop around that box (+15% margin) at native resolution (long side <= 1024 px).
Stage 2  Grounding DINO-base again inside the crop, the same rule restricted to boxes centred in the middle 60% ->
         SAM 2.1-B prompted with the box, two jittered boxes (prompt stability) and the stage-1 box -> identical
         clean-up -> quality metrics -> core traits.

All functions take plain numpy RGB images, so they can be called from R through reticulate.
"""
import numpy as np
import cv2
import torch
from PIL import Image as PILImage

from .colour import colour_name
from .shape import radial_metrics

DEFAULTS = dict(
    detector="IDEA-Research/grounding-dino-base", prompts=["flower"], det_threshold=0.05,
    sam_weights="sam2.1_b.pt", jitter=[0.9, 1.1],
    photo_max_side=2048, crop_margin=0.15, crop_max_side=1024,
    stage1_size={"shortest_edge": 480, "longest_edge": 800},
    stage2_size=None,  # model default (800/1333)
    two_stage=True, rel_score=0.85, cluster_n=2,
    other_min_score=0.30, edge_tol=0.01, fp16=False,
)
YW = np.array([0.2126, 0.7152, 0.0722], np.float32)
CORE = ["D_px", "L_mean", "L_sd", "C_mean", "a_mean", "b_mean", "hue_deg", "hue_conc", "vap_wp", "dark_frac_otsu",
        "L_entropy", "centre_dL", "centre_dE", "petal_L", "petal_a", "petal_b", "sym_bilateral", "sym_rotational",
        "petal_n", "lobe_depth", "radius_cv", "fill_circle", "aspect", "solidity_shape", "colour_name"]


class Models:
    """Detector + segmenter, loaded once."""

    def __init__(self, cfg=None, device=None):
        from transformers import AutoModelForZeroShotObjectDetection, AutoProcessor
        from ultralytics import SAM
        self.cfg = {**DEFAULTS, **(cfg or {})}
        self.device = device or ("cuda" if torch.cuda.is_available() else "cpu")
        self.proc = AutoProcessor.from_pretrained(self.cfg["detector"])
        self.det = AutoModelForZeroShotObjectDetection.from_pretrained(self.cfg["detector"]).to(self.device).eval()
        self.sam = SAM(self.cfg["sam_weights"])


# ---------- image helpers ----------
def load_rgb(path, max_side=None):
    """Read a photo as RGB uint8 (non-ASCII Windows paths work); optional area-averaged reduction."""
    buf = np.fromfile(str(path), np.uint8)
    img = cv2.imdecode(buf, cv2.IMREAD_COLOR) if buf.size else None
    if img is None:
        return None
    img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
    if max_side:
        s = max_side / max(img.shape[:2])
        if s < 1:
            img = cv2.resize(img, None, fx=s, fy=s, interpolation=cv2.INTER_AREA)
    return img


def scale_box(b, f, w, h):
    cx, cy, bw, bh = (b[0] + b[2]) / 2, (b[1] + b[3]) / 2, (b[2] - b[0]) * f, (b[3] - b[1]) * f
    return [max(0.0, cx - bw / 2), max(0.0, cy - bh / 2), min(float(w), cx + bw / 2), min(float(h), cy + bh / 2)]


def iou(a, b):
    u = (a | b).sum()
    return float((a & b).sum() / u) if u else 0.0


def _inside_frac(inner, outer):
    """Share of box `inner` that lies inside box `outer`."""
    x1, y1 = max(inner[0], outer[0]), max(inner[1], outer[1])
    x2, y2 = min(inner[2], outer[2]), min(inner[3], outer[3])
    a = (inner[2] - inner[0]) * (inner[3] - inner[1])
    return max(0.0, x2 - x1) * max(0.0, y2 - y1) / a if a > 0 else 0.0


# ---------- detection ----------
def detect(models, imgs, size=None):
    """Grounding DINO on a list of RGB images -> list of (boxes Nx4 px, scores N)."""
    cfg, dev = models.cfg, models.device
    th, text = cfg["det_threshold"], ". ".join(p.lower() for p in cfg["prompts"]) + "."
    kw = {"size": size} if size else {}
    inp = models.proc(images=[PILImage.fromarray(i) for i in imgs], text=[text] * len(imgs), return_tensors="pt", **kw).to(dev)
    with torch.no_grad(), torch.autocast(dev, dtype=torch.float16, enabled=bool(cfg["fp16"]) and dev == "cuda"):
        out = models.det(**inp)
    sizes = [i.shape[:2] for i in imgs]
    try:
        res = models.proc.post_process_grounded_object_detection(out, inp["input_ids"], threshold=th, text_threshold=th,
                                                                 target_sizes=sizes)
    except TypeError:
        res = models.proc.post_process_grounded_object_detection(out, inp["input_ids"], box_threshold=th, text_threshold=th,
                                                                 target_sizes=sizes)
    outs = []
    for r, (h, w) in zip(res, sizes):
        b = r["boxes"].detach().float().cpu().numpy().reshape(-1, 4)
        s = r["scores"].detach().float().cpu().numpy().reshape(-1)
        b[:, [0, 2]] = b[:, [0, 2]].clip(0, w)
        b[:, [1, 3]] = b[:, [1, 3]].clip(0, h)
        outs.append((b, s))
    return outs


def pick_flower(b, s, w, h, cfg, central=False):
    """Main flower among detections: the largest box scoring >= rel_score * best score (optionally only boxes
    centred in the middle 60% of the image), skipping boxes that enclose >= cluster_n other confident flowers."""
    if len(s) == 0:
        return None, 0.0
    idx = np.arange(len(s))
    if central:
        cx, cy = (b[:, 0] + b[:, 2]) / 2, (b[:, 1] + b[:, 3]) / 2
        c = idx[(abs(cx - w / 2) < 0.3 * w) & (abs(cy - h / 2) < 0.3 * h)]
        idx = c if len(c) else idx
    best = s[idx].max()
    cand = idx[s[idx] >= cfg["rel_score"] * best]
    area = (b[cand, 2] - b[cand, 0]) * (b[cand, 3] - b[cand, 1])
    conf = np.where(s >= max(cfg["other_min_score"], 0.5 * best))[0]
    for k in cand[np.argsort(-area)]:
        inner = sum(1 for j in conf if j != k and _inside_frac(b[j], b[k]) > 0.8
                    and (b[j, 2] - b[j, 0]) * (b[j, 3] - b[j, 1]) < 0.6 * (b[k, 2] - b[k, 0]) * (b[k, 3] - b[k, 1]))
        if inner < cfg["cluster_n"]:
            return [float(v) for v in b[k]], float(s[k])
    k = cand[np.argmax(s[cand])]
    return [float(v) for v in b[k]], float(s[k])


def localize(b, s, w, h, cfg):
    """Stage 1 on a full photo: main flower box, number of other confident flowers (boxes less than half inside
    the main box) and whether the main box touches the photo frame."""
    box, score = pick_flower(b, s, w, h, cfg)
    if box is None:
        return None, 0.0, 0, False
    others = sum(1 for j in np.where(s >= cfg["other_min_score"])[0]
                 if _inside_frac(b[j], box) < 0.5 and not np.allclose(b[j], box))
    tol = cfg["edge_tol"]
    cut = bool(box[0] <= tol * w or box[1] <= tol * h or box[2] >= (1 - tol) * w or box[3] >= (1 - tol) * h)
    return box, score, int(others), cut


def square_crop(img, box, cfg):
    """Square crop around the box with a margin, shifted inside the photo where possible; long side <= crop_max_side."""
    H, W = img.shape[:2]
    cx, cy = (box[0] + box[2]) / 2, (box[1] + box[3]) / 2
    side = max(box[2] - box[0], box[3] - box[1]) * (1 + 2 * cfg["crop_margin"])
    side = min(side, max(H, W))
    x0 = int(round(min(max(0, cx - side / 2), max(0, W - side))))
    y0 = int(round(min(max(0, cy - side / 2), max(0, H - side))))
    x1, y1 = int(round(min(W, x0 + side))), int(round(min(H, y0 + side)))
    crop = img[y0:y1, x0:x1]
    s = min(1.0, cfg["crop_max_side"] / max(crop.shape[:2]))
    if s < 1:
        crop = cv2.resize(crop, None, fx=s, fy=s, interpolation=cv2.INTER_AREA)
    return crop, (x0, y0, x1, y1), s


# ---------- segmentation ----------
def clean(m, box=None, pad=0.1):
    """Identical post-processing for every mask: clip to box (+10%), largest component, holes filled."""
    m = m.astype(np.uint8)
    if box is not None:
        x1, y1, x2, y2 = box
        px, py = pad * (x2 - x1), pad * (y2 - y1)
        keep = np.zeros_like(m)
        keep[max(0, int(y1 - py)):int(y2 + py) + 1, max(0, int(x1 - px)):int(x2 + px) + 1] = 1
        m &= keep
    n, lab, st, _ = cv2.connectedComponentsWithStats(m, 8)
    if n <= 1:
        return np.zeros(m.shape, bool)
    m = (lab == 1 + np.argmax(st[1:, cv2.CC_STAT_AREA])).astype(np.uint8)
    cn, _ = cv2.findContours(m, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
    out = np.zeros_like(m)
    cv2.drawContours(out, cn, -1, 1, -1)
    return out.astype(bool)


def segment(models, img, boxes):
    """SAM 2.1 with several box prompts on one image embedding -> cleaned masks and SAM predicted IoUs (same order)."""
    h, w = img.shape[:2]
    r = models.sam(cv2.cvtColor(img, cv2.COLOR_RGB2BGR), bboxes=[list(map(float, b)) for b in boxes], verbose=False)[0]
    masks, conf = [np.zeros((h, w), bool) for _ in boxes], [0.0] * len(boxes)
    if r.masks is not None and len(r.masks.data):
        for d, c, s in zip(r.masks.data.cpu().numpy(), r.boxes.cls.cpu().numpy().astype(int), r.boxes.conf.cpu().numpy()):
            masks[c] = cv2.resize(d.astype(np.uint8), (w, h), interpolation=cv2.INTER_NEAREST).astype(bool)
            conf[c] = float(s)
    return [clean(m, b) for m, b in zip(masks, boxes)], conf


# ---------- core traits (reproduced in R by ft_traits()) ----------
def prep(img):
    x = img.astype(np.float32) / 255
    lin = np.where(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055) ** 2.4).astype(np.float32)
    lab = cv2.cvtColor(lin, cv2.COLOR_LRGB2Lab)
    Y = lin @ YW
    return dict(lab=lab, Y=Y, y99=float(np.percentile(Y, 99)) + 1e-6, L8=np.clip(lab[..., 0] * 2.55, 0, 255).astype(np.uint8))


def traits(P, m, min_px=600):
    """Core colour, radial-pattern and symmetry traits of one flower outline (crop at <= 1024 px)."""
    ys, xs = np.nonzero(m)
    if len(ys) < min_px:
        return {}
    D = 2 * np.sqrt(len(ys) / np.pi)
    lab = P["lab"]
    L, a, b = lab[..., 0][m], lab[..., 1][m], lab[..., 2][m]
    C, th = np.hypot(a, b), np.arctan2(b, a)
    hx, hy = (C * np.cos(th)).sum() / (C.sum() + 1e-6), (C * np.sin(th)).sum() / (C.sum() + 1e-6)
    L8 = np.clip(L * 2.55, 0, 255).astype(np.uint8).reshape(1, -1)
    thr, _ = cv2.threshold(L8, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
    hist, _ = np.histogram(L, 50, (0, 100))
    p = hist[hist > 0] / hist.sum()
    out = dict(D_px=round(D, 1), L_mean=float(L.mean()), L_sd=float(L.std()), C_mean=float(C.mean()),
               a_mean=float(a.mean()), b_mean=float(b.mean()),
               hue_deg=float(np.degrees(np.arctan2(hy, hx)) % 360), hue_conc=float(np.hypot(hx, hy)),
               vap_wp=float(1 - min(1, P["Y"][m].mean() / P["y99"])),
               dark_frac_otsu=float((L8 <= thr).mean()), L_entropy=float(-(p * np.log2(p)).sum()))
    out.update(radial_metrics(lab, m))
    out["colour_name"] = (str(colour_name(out["petal_L"], out["petal_a"], out["petal_b"]))
                          if np.isfinite(out["petal_L"]) else "")
    return out


def quality(img, m):
    """Photo/outline quality covariates (not traits)."""
    if not m.any():
        return {}
    cn, _ = cv2.findContours(m.astype(np.uint8), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
    c = max(cn, key=cv2.contourArea)
    hull = cv2.contourArea(cv2.convexHull(c))
    (_, _), (rw, rh), _ = cv2.minAreaRect(c)
    pix = img[m]
    inner = cv2.erode(m.astype(np.uint8), np.ones((5, 5), np.uint8)).astype(bool)
    lap = cv2.Laplacian(cv2.cvtColor(img, cv2.COLOR_RGB2GRAY), cv2.CV_32F)
    return dict(solidity=float(m.sum() / hull) if hull else np.nan,
                elong=float(max(rw, rh) / max(1e-6, min(rw, rh))),
                clip_hi=float((pix.max(1) >= 250).mean()), clip_lo=float((pix.max(1) <= 5).mean()),
                sharp=float(lap[inner].var()) if inner.sum() > 50 else np.nan)


# ---------- photo -> crop -> outline -> traits ----------
def process_photo(models, photo, two_stage=None):
    """photo: RGB array (full photo). Returns (record dict, crop RGB, mask bool, extra masks dict)."""
    cfg = models.cfg
    two_stage = cfg["two_stage"] if two_stage is None else two_stage
    H, W = photo.shape[:2]
    rec = dict(photo_w=W, photo_h=H)
    (b1, s1), = detect(models, [photo], cfg["stage1_size"])
    box1, score1, others, cut = localize(b1, s1, W, H, cfg)
    rec.update(det1_score=round(score1, 4), n_det1=int(len(s1)), n_other_flowers=others, cut_by_frame=cut)
    if box1 is None:
        box1 = [0.03 * W, 0.03 * H, 0.97 * W, 0.97 * H]
    crop, (x0, y0, x1, y1), s = square_crop(photo, box1, cfg)
    h, w = crop.shape[:2]
    box1c = [max(0.0, (box1[0] - x0) * s), max(0.0, (box1[1] - y0) * s), min(float(w), (box1[2] - x0) * s),
             min(float(h), (box1[3] - y0) * s)]
    rec.update(crop_x0=x0, crop_y0=y0, crop_x1=x1, crop_y1=y1, crop_scale=round(s, 4), W=w, H=h,
               box1_frac=round((box1[2] - box1[0]) * (box1[3] - box1[1]) / (W * H), 4),
               flower_px_native=round(max(box1[2] - box1[0], box1[3] - box1[1]), 1))
    if two_stage:
        (b2, s2), = detect(models, [crop], cfg["stage2_size"])
        box, score = pick_flower(b2, s2, w, h, cfg, central=True)
        box = box or [0.03 * w, 0.03 * h, 0.97 * w, 0.97 * h]
        rec.update(n_det=int(len(s2)))
    else:
        box, score = box1c, score1
    rec.update(det_score=round(score, 4), x1=round(box[0], 1), y1=round(box[1], 1), x2=round(box[2], 1), y2=round(box[3], 1))
    prompts = [box] + [scale_box(box, f, w, h) for f in cfg["jitter"]] + ([box1c] if two_stage else [])
    ms, conf = segment(models, crop, prompts)
    m = ms[0]
    k = len(cfg["jitter"])
    stab = min(iou(m, x) for x in ms[1:1 + k]) if m.any() else 0.0
    bt = float(np.concatenate([m[0], m[-1], m[:, 0], m[:, -1]]).mean()) if m.any() else np.nan
    rec.update(sam_iou=round(conf[0], 4), stab=round(stab, 4), px=int(m.sum()), mask_frac=round(float(m.mean()), 4),
               border_touch=bt, mask_ok=bool(score > 0 and m.sum() >= 600 and bt <= 0.10))
    extra = {}
    if two_stage:
        extra["stage1"] = ms[-1]
        rec["iou_stage1"] = round(iou(m, ms[-1]), 4)
    rec.update(quality(crop, m))
    rec.update(traits(prep(crop), m))
    return rec, crop, m, extra
