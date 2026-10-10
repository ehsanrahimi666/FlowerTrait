"""Radial colour pattern, symmetry and petal-number traits of a flower outline.

Every computation here is reproduced exactly in R (ft_traits(), ft_symmetry()), so it uses plain array arithmetic:
an area-averaging resize ported from the R code, a radius function sampled along 360 rays from the centroid,
a discrete Fourier transform of that function and nearest-pixel rotations and reflections of the outline.

Geometric traits are computed on the outline reduced so that its equivalent diameter D is at most WORK_D px
(a fixed working resolution makes them comparable between photographs taken at different distances).
"""
import numpy as np

from .colour import ciede2000

WORK_D = 128.0
N_RAY = 360
K_MIN, K_MAX = 3, 24          # petal-number search range (harmonics of the radius function)
CENTRE, PERIPHERY = 0.3, 0.6  # relative radius limits of the centre and of the petal periphery
AXES = 36                     # mirror axes tested for bilateral symmetry (every 5 degrees)

GEOM_KEYS = ["sym_bilateral", "sym_axis_deg", "sym_rotational", "petal_n", "petal_power", "lobe_depth", "radius_cv",
             "fill_circle", "aspect", "solidity_shape"]
RADIAL_KEYS = ["centre_dL", "centre_dE", "centre_frac", "petal_L", "petal_a", "petal_b"]


def _area_rows(M, n_out):
    """Area-averaging resize along the first axis (exact pixel-overlap weights from cumulative sums).
    Port of the R function .ft_area_rows(), so that both languages give the same working outline."""
    n_in = M.shape[0]
    if n_out == n_in:
        return M.astype(float)
    s = n_in / n_out
    C = np.vstack([np.zeros((1, M.shape[1])), np.cumsum(M.astype(float), axis=0)])
    e = np.arange(n_out + 1) * s
    k = np.minimum(np.floor(e), n_in - 1).astype(int)
    S = C[k] + (e - k)[:, None] * M[k].astype(float)
    return (S[1:] - S[:-1]) / s


def area_resize(M, h, w):
    return _area_rows(_area_rows(M, h).T, w).T


def work_mask(m):
    """Outline reduced (area averaging, threshold 0.5) so that D <= WORK_D; returns (mask, scale)."""
    D = 2 * np.sqrt(m.sum() / np.pi)
    if D <= WORK_D:
        return m.astype(bool), 1.0
    s = WORK_D / D
    h, w = max(1, int(np.floor(m.shape[0] * s + 0.5))), max(1, int(np.floor(m.shape[1] * s + 0.5)))
    return area_resize(m.astype(float), h, w) > 0.5, s


def _centroid(m):
    ys, xs = np.nonzero(m)
    return ys.mean(), xs.mean(), ys, xs


def radius_function(m, cy, cx, n_ray=N_RAY, step=0.25):
    """Distance from the centroid to the farthest outline pixel along n_ray rays (pixel lookup by rounding)."""
    h, w = m.shape
    th = 2 * np.pi * np.arange(n_ray) / n_ray
    dmax = np.hypot(h, w)
    d = np.arange(0, dmax + step, step)
    yy = np.floor(cy + np.outer(np.sin(th), d) + 0.5).astype(int)
    xx = np.floor(cx + np.outer(np.cos(th), d) + 0.5).astype(int)
    ok = (yy >= 0) & (yy < h) & (xx >= 0) & (xx < w)
    hit = np.zeros(yy.shape, bool)
    hit[ok] = m[yy[ok], xx[ok]]
    r = np.where(hit.any(1), d[np.where(hit, np.arange(len(d))[None, :], -1).max(1)], 0.0)
    return r


def _overlap(m, ys, xs, ty, tx):
    """IoU between the outline and its transformed copy (transformed pixel centres looked up by rounding)."""
    h, w = m.shape
    ry, rx = np.floor(ty + 0.5).astype(int), np.floor(tx + 0.5).astype(int)
    ok = (ry >= 0) & (ry < h) & (rx >= 0) & (rx < w)
    hit = np.zeros(len(ys), bool)
    hit[ok] = m[ry[ok], rx[ok]]
    n = hit.sum()
    return float(n / (2 * len(ys) - n))


def geometry(m):
    """Symmetry, petal number and outline shape on the working outline."""
    out = dict.fromkeys(GEOM_KEYS, np.nan)
    wm, s = work_mask(m)
    if wm.sum() < 100:
        return out, None
    cy, cx, ys, xs = _centroid(wm)
    r = radius_function(wm, cy, cx)
    r0 = r.mean()
    if r0 <= 0:
        return out, None
    F = np.abs(np.fft.rfft(r)) / len(r)
    ks = np.arange(K_MIN, K_MAX + 1)
    kk = int(ks[np.argmax(F[ks])])
    dy, dx = ys - cy, xs - cx
    # rotation by one petal (360 / petal_n degrees)
    a = 2 * np.pi / kk
    rot = _overlap(wm, ys, xs, cy + dx * np.sin(a) + dy * np.cos(a), cx + dx * np.cos(a) - dy * np.sin(a))
    # best mirror axis through the centroid: every 5 degrees, then +-5 degrees around the best in 0.5-degree steps
    def mirror(deg):
        phi = np.radians(deg)
        ux, uy = np.cos(phi), np.sin(phi)
        t = dx * ux + dy * uy
        return _overlap(wm, ys, xs, cy + 2 * t * uy - dy, cx + 2 * t * ux - dx)
    best, best_ax = -1.0, np.nan
    for deg in [180.0 * j / AXES for j in range(AXES)]:
        v = mirror(deg)
        if v > best:
            best, best_ax = v, deg
    for deg in [best_ax + 0.5 * i for i in range(-10, 11) if i != 0]:
        v = mirror(deg)
        if v > best:
            best, best_ax = v, deg % 180
    # second moments, fill of the circumscribed circle, solidity (convex hull of row extremes)
    cov = np.cov(np.vstack([xs, ys]), bias=True)
    ev = np.sort(np.linalg.eigvalsh(cov))[::-1]
    out.update(sym_bilateral=best, sym_axis_deg=best_ax, sym_rotational=rot, petal_n=kk,
               petal_power=float(F[kk] / F[1:].sum()), lobe_depth=float(2 * F[kk] / r0),
               radius_cv=float(r.std() / r0), fill_circle=float(len(ys) / (np.pi * r.max() ** 2)),
               aspect=float(np.sqrt(ev[0] / max(ev[1], 1e-12))), solidity_shape=_solidity(wm))
    return out, (r, cy, cx, s)


def _hull_area(x, y):
    pts = sorted(set(zip(x.tolist(), y.tolist())))
    if len(pts) < 3:
        return 0.0

    def cross(o, a, b):
        return (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0])
    lower, upper = [], []
    for p in pts:
        while len(lower) >= 2 and cross(lower[-2], lower[-1], p) <= 0:
            lower.pop()
        lower.append(p)
    for p in reversed(pts):
        while len(upper) >= 2 and cross(upper[-2], upper[-1], p) <= 0:
            upper.pop()
        upper.append(p)
    hull = np.array(lower[:-1] + upper[:-1], float)
    xh, yh = hull[:, 0], hull[:, 1]
    return float(abs(np.sum(xh * np.roll(yh, -1) - np.roll(xh, -1) * yh)) / 2)


def _solidity(m):
    """Pixel count / area of the convex hull of the first and last pixel centre of every row."""
    ys, xs = np.nonzero(m)
    first = np.r_[True, ys[1:] != ys[:-1]]
    last = np.r_[ys[1:] != ys[:-1], True]
    hx = np.r_[xs[first], xs[last]].astype(float)
    hy = np.r_[ys[first], ys[last]].astype(float)
    a = _hull_area(hx, hy)
    return float(m.sum() / a) if a > 0 else np.nan


def radial_colour(lab, m, rad):
    """Centre versus petal-periphery colour. A pixel's relative radius is its distance from the centroid divided
    by the outline radius in its direction; the centre is < CENTRE and the periphery > PERIPHERY."""
    out = dict.fromkeys(RADIAL_KEYS, np.nan)
    if rad is None:
        return out
    r, _, _, s = rad
    ys, xs = np.nonzero(m)
    cy, cx = ys.mean(), xs.mean()
    ang = np.arctan2(ys - cy, xs - cx) % (2 * np.pi)
    j = np.floor(ang / (2 * np.pi) * N_RAY + 0.5).astype(int) % N_RAY
    rr = r[j] / s
    rel = np.hypot(ys - cy, xs - cx) / np.maximum(rr, 1e-9)
    cen, per = rel < CENTRE, rel > PERIPHERY
    if cen.sum() < 20 or per.sum() < 20:
        return out
    L, a, b = lab[..., 0][ys, xs].astype(float), lab[..., 1][ys, xs].astype(float), lab[..., 2][ys, xs].astype(float)
    c = np.array([L[cen].mean(), a[cen].mean(), b[cen].mean()])
    p = np.array([L[per].mean(), a[per].mean(), b[per].mean()])
    out.update(centre_dL=float(c[0] - p[0]), centre_dE=float(ciede2000(c, p)), centre_frac=float(cen.mean()),
               petal_L=float(p[0]), petal_a=float(p[1]), petal_b=float(p[2]))
    return out


def radial_metrics(lab, m):
    """All radial-pattern and geometric traits for a CIELAB crop and its outline."""
    g, rad = geometry(m)
    out = dict(g)
    out.update(radial_colour(lab, m, rad))
    return out
