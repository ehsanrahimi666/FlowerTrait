# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
"""Contact sheets of crops with outlines (and optionally the whole photo) for visual labelling."""
import os
import sys

import cv2
import numpy as np


def tile_img(crop_path, mask_path, tile=260, photo_path=None, label="", col=(255, 215, 0)):
    c = cv2.imdecode(np.fromfile(crop_path, np.uint8), cv2.IMREAD_COLOR)
    c = cv2.cvtColor(c, cv2.COLOR_BGR2RGB)
    if mask_path and os.path.exists(mask_path):
        m = cv2.imdecode(np.fromfile(mask_path, np.uint8), cv2.IMREAD_GRAYSCALE) > 0
        if m.shape != c.shape[:2]:
            m = cv2.resize(m.astype(np.uint8), (c.shape[1], c.shape[0]), interpolation=cv2.INTER_NEAREST) > 0
        cn, _ = cv2.findContours(m.astype(np.uint8), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
        c = c.copy()
        c[~m] = (c[~m] * 0.4).astype(np.uint8)  # darken everything outside the outline
        lw = max(2, c.shape[1] // 200)
        c = cv2.drawContours(c, cn, -1, (0, 0, 0), lw + 2)
        c = cv2.drawContours(c, cn, -1, (0, 255, 255), lw)
    s = tile / max(c.shape[:2])
    c = cv2.resize(c, (max(1, int(c.shape[1] * s)), max(1, int(c.shape[0] * s))), interpolation=cv2.INTER_AREA)
    canvas = np.full((tile + 22, tile, 3), 255, np.uint8)
    canvas[:c.shape[0], :c.shape[1]] = c
    if photo_path and os.path.exists(photo_path):
        p = cv2.cvtColor(cv2.imdecode(np.fromfile(photo_path, np.uint8), cv2.IMREAD_COLOR), cv2.COLOR_BGR2RGB)
        ps = (tile // 3) / max(p.shape[:2])
        p = cv2.resize(p, (max(1, int(p.shape[1] * ps)), max(1, int(p.shape[0] * ps))), interpolation=cv2.INTER_AREA)
        canvas[tile - p.shape[0]:tile, tile - p.shape[1]:tile] = p
        cv2.rectangle(canvas, (tile - p.shape[1], tile - p.shape[0]), (tile - 1, tile - 1), (255, 255, 255), 1)
    cv2.putText(canvas, str(label)[:34], (3, tile + 16), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (0, 0, 0), 1, cv2.LINE_AA)
    return canvas


def sheet(items, out, ncol=6, tile=260):
    """items: list of dicts with crop, mask, photo (optional) and label."""
    tiles = [tile_img(i["crop"], i.get("mask"), tile, i.get("photo"), i.get("label", "")) for i in items]
    blank = np.full_like(tiles[0], 255)
    rows = [np.hstack(tiles[k:k + ncol] + [blank] * (ncol - len(tiles[k:k + ncol]))) for k in range(0, len(tiles), ncol)]
    cv2.imencode(".jpg", cv2.cvtColor(np.vstack(rows), cv2.COLOR_RGB2BGR), [cv2.IMWRITE_JPEG_QUALITY, 88])[1].tofile(out)
    return out


if __name__ == "__main__":
    import pandas as pd
    run = sys.argv[1]
    d = pd.read_csv(os.path.join(run, "traits.csv"), dtype={"photo_id": str})
    items = [dict(crop=os.path.join(run, "crops", f"{p}.jpg"), mask=os.path.join(run, "masks", f"{p}.png"),
                  photo=os.path.join(run, "photos", f"{p}.jpg") if os.path.exists(os.path.join(run, "photos", f"{p}.jpg")) else None,
                  label=f"{i} {p[:22]}") for i, p in enumerate(d.photo_id) if os.path.exists(os.path.join(run, "crops", f"{p}.jpg"))]
    print(sheet(items, sys.argv[2], ncol=int(sys.argv[3]) if len(sys.argv) > 3 else 6))
