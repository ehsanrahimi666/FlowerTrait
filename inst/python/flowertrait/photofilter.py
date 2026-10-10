"""Photo filter: is this flower photo usable for trait extraction, and does it show the flower face-on?

Heads (small logistic regressions) were trained on visually labelled iNaturalist photos of flowering plants
(see the FlowerTrait R package documentation). Features: OpenCLIP ViT-B/16 and DINOv2-small embeddings of the
native crop and of the flower-only crop (grey background), zero-shot text similarities, and outline and quality
metrics from photo.process_photo.

    from flowertrait.photo import Models, load_rgb, process_photo
    from flowertrait.photofilter import PhotoFilter
    M, F = Models(), PhotoFilter("filter_heads.json")
    rec, crop, mask, _ = process_photo(M, load_rgb("photo.jpg", 2048))
    rec.update(F.predict(crop, mask, rec))
    rec.update(F.decide(rec))
"""
import json

import cv2
import numpy as np
import torch
from PIL import Image

DEFAULT_THRESHOLDS = dict(p_usable=0.5, p_face_view=0.5, stab=None)  # stab=0.8 gives the strict rule
SHAPE_KEYS = ("sym_bilateral", "sym_rotational", "lobe_depth", "radius_cv", "fill_circle", "aspect", "solidity_shape",
              "shape_circularity", "shape_roughness")


class PhotoFilter:
    def __init__(self, heads_json, device=None):
        import open_clip
        from transformers import AutoImageProcessor, AutoModel
        with open(heads_json) as f:
            self.spec = json.load(f)
        self.device = device or ("cuda" if torch.cuda.is_available() else "cpu")
        self.clip, _, self.pre = open_clip.create_model_and_transforms(self.spec["clip_arch"],
                                                                       pretrained=self.spec["clip_pretrained"])
        self.clip = self.clip.to(self.device).eval()
        tok = open_clip.get_tokenizer(self.spec["clip_arch"])
        with torch.no_grad():
            T = []
            for ps in self.spec["prompts"].values():
                e = self.clip.encode_text(tok(ps).to(self.device))
                e = (e / e.norm(dim=-1, keepdim=True)).mean(0)
                T.append((e / e.norm()).cpu().numpy())
        self.text = np.stack(T, 1)
        self.dproc = AutoImageProcessor.from_pretrained(self.spec["dino"])
        self.dino = AutoModel.from_pretrained(self.spec["dino"]).to(self.device).eval()

    @staticmethod
    def masked_crop(crop, mask):
        g = crop.copy()
        g[~mask] = 128
        ys, xs = np.nonzero(mask)
        if len(ys):
            pad = int(0.05 * max(np.ptp(ys), np.ptp(xs))) + 2
            g = g[max(0, ys.min() - pad):ys.max() + pad + 1, max(0, xs.min() - pad):xs.max() + pad + 1]
        return g

    @torch.no_grad()
    def _embed(self, rgb):
        pil = Image.fromarray(rgb)
        c = self.clip.encode_image(self.pre(pil)[None].to(self.device))
        c = (c / c.norm(dim=-1, keepdim=True)).cpu().numpy()[0]
        o = self.dino(**self.dproc(images=[pil], return_tensors="pt").to(self.device))
        d = torch.cat([o.last_hidden_state[:, 0], o.last_hidden_state[:, 1:].mean(1)], 1)
        d = (d / d.norm(dim=-1, keepdim=True)).cpu().numpy()[0]
        return c, d

    def _qc_vector(self, rec, shape, zs):
        q = {**{k: rec.get(k, np.nan) for k in self.spec["qc_features"]}, **shape}
        q["log_sharp"] = np.log1p(rec.get("sharp", np.nan))
        q["log_px"] = np.log(max(1.0, rec.get("flower_px_native", np.nan) or 1.0))
        for k, v in zip(self.spec["prompts"], zs):
            q["zs_" + k] = v
        x = np.array([float(q.get(k, np.nan)) for k in self.spec["qc_features"]], float)
        med = np.array(self.spec.get("qc_medians", [0.0] * len(x)), float)
        x[np.isnan(x)] = med[np.isnan(x)]
        return x

    def features(self, crop, mask, rec, ext=None):
        """Feature sets used by the heads (also saved by run_photos --embeddings for re-training)."""
        if ext is None:
            from .pipeline import extract_traits
            s = 768 / max(crop.shape[:2])
            c768, m768 = (cv2.resize(crop, None, fx=s, fy=s, interpolation=cv2.INTER_AREA),
                          cv2.resize(mask.astype(np.uint8), None, fx=s, fy=s, interpolation=cv2.INTER_NEAREST) > 0) if s < 1 else (crop, mask)
            ext = extract_traits(c768, m768) if m768.sum() >= 600 else {}
        shape = {k: ext.get(k, rec.get(k, np.nan)) for k in SHAPE_KEYS}
        cf, df = self._embed(crop)
        cm, dm = self._embed(self.masked_crop(crop, mask)) if mask.any() else (cf, df)
        zs = cf @ self.text
        q = self._qc_vector(rec, shape, zs)
        return {"QC+shape": q, "clip": cf, "dino": df, "clip+dino": np.r_[cf, df], "clip+dino(masked)": np.r_[cm, dm],
                "clip+dino+QC": np.r_[cf, df, q], "zeroshot": zs}

    def predict(self, crop, mask, rec, ext=None):
        """Probabilities for each head (p_usable, p_face_view, p_open_flower, p_fully_visible, p_sharp)."""
        sets = self.features(crop, mask, rec, ext)
        out = {}
        for t, h in self.spec.get("heads", {}).items():
            x = (sets[h["features"]] - np.array(h["mean"])) / np.array(h["scale"])
            out[f"p_{t}"] = float(1 / (1 + np.exp(-(x @ np.array(h["coef"]) + h["intercept"]))))
        return out

    @staticmethod
    def decide(rec, thresholds=None):
        """Keep/reject with a reason and the view (face or side) of kept photos (default rule; stab adds the
        strict rule)."""
        th = {**DEFAULT_THRESHOLDS, **(thresholds or {})}
        if not rec.get("mask_ok", False):
            return dict(keep=False, reason="outline failed QC")
        if th["stab"] is not None and rec.get("stab", 0) < th["stab"]:
            return dict(keep=False, reason="unstable outline")
        if rec.get("p_usable", 0) < th["p_usable"]:
            return dict(keep=False, reason="unclear photo (no open flower, hidden, blurry or cut off)")
        view = "face" if rec.get("p_face_view", 1) >= th["p_face_view"] else "side"
        return dict(keep=True, reason=f"{view} view", view=view)
