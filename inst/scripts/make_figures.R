# Figures of the FlowerTrait article, made from the data and photographs shipped with the package.
# Usage: Rscript make_figures.R OUTPUT_FOLDER
# Photographs: CC0 / CC BY validation photographs from iNaturalist via GBIF; attribution in
# inst/extdata/ATTRIBUTION.csv and inst/extdata/gallery/ATTRIBUTION.csv. Uses the 'maps' package for the world
# map of Figure 1 (an empty frame is drawn when it is not installed).
library(FlowerTrait)

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "figures"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------- style
ink <- "#1D2731"; ink2 <- "#5B6770"; grid_col <- "#E6E9EC"
c_keep <- "#2E7D5B"; c_rej <- "#B5443B"; c_hi <- "#E8A33D"; c_mid <- "#3E6A9E"; c_light <- "#DDE7F2"
region_col <- c("Europe" = "#2F6DB5", "Middle East" = "#E08E2B", "Africa" = "#B5443B",
                "North America" = "#2E8B57", "South America" = "#8E5DB5", "Oceania" = "#C9A227")
region_lab <- c("Europe" = "Europe", "Middle East" = "Eastern Mediterranean", "Africa" = "Southern Africa",
                "North America" = "North America", "South America" = "South America", "Oceania" = "Australia")
fam_col <- c(colour = "#C0504D", pattern = "#3E6A9E", symmetry = "#8E5DB5", shape = "#2E8B57")
verdict_col <- c("reliable" = "#2E7D5B", "use with care" = "#E8A33D", "not reliable" = "#B5443B")
name_col <- c(white = "#F4F4F4", grey = "#9E9E9E", black = "#262626", red = "#D7301F", pink = "#F28CB1",
              orange = "#F28E2B", brown = "#8C5A3C", yellow = "#F6D32D", green = "#4DAF4A", blue = "#4C72B0",
              purple = "#8E5DB5")
name_lev <- names(name_col)

fig <- function(name, w, h, expr) {
  grDevices::png(file.path(out, name), width = w, height = h, units = "in", res = 300, type = "cairo")
  op <- graphics::par(family = "sans", cex = 0.8, col.axis = ink2, col.lab = ink, fg = ink2, tcl = -0.25,
                      mgp = c(2.1, 0.45, 0), las = 1)
  on.exit({graphics::par(op); grDevices::dev.off()})
  if (nzchar(Sys.getenv("FT_FIG_TRY"))) {
    tryCatch(force(expr), error = function(e) message(name, ": ", conditionMessage(e)))
  } else {
    force(expr)
  }
  invisible(file.path(out, name))
}
plab <- function(x, line = 0.4, adj = -0.02) mtext(x, side = 3, adj = adj, line = line, font = 2, cex = 1.05, col = ink)
ptitle <- function(x, line = 0.35, cex = 0.72) mtext(x, side = 3, line = line, cex = cex, col = ink)
gridlines <- function(h = NULL, v = NULL) abline(h = h, v = v, col = grid_col, lwd = 0.8)
lab_hex <- function(L, a, b) FlowerTrait:::.ft_lab_to_hex(cbind(L, a, b))
draw_img <- function(img, border = NA, lwd = 2, square = FALSE) {
  h <- dim(img)[1]; w <- dim(img)[2]
  s <- max(h, w)
  x0 <- if (square) (s - w) / 2 else 0
  y0 <- if (square) (s - h) / 2 else 0
  plot.new()
  if (square) plot.window(c(0, s), c(0, s), asp = 1, xaxs = "i", yaxs = "i") else
    plot.window(c(0, w), c(0, h), asp = 1, xaxs = "i", yaxs = "i")
  rasterImage(as.raster(img / 255), x0, y0, x0 + w, y0 + h)
  if (!is.na(border)) rect(x0, y0, x0 + w, y0 + h, border = border, lwd = lwd, xpd = NA)
  invisible(c(h = h, w = w, x0 = x0, y0 = y0))
}
contour_of <- function(m) {
  h <- nrow(m); w <- ncol(m)
  z <- matrix(0, w + 2, h + 2)
  z[2:(w + 1), 2:(h + 1)] <- t(m[h:1, , drop = FALSE])
  contourLines((0:(w + 1)) - 0.5, (0:(h + 1)) - 0.5, z, levels = 0.5)
}
outline <- function(m, col = "#FFFFFF", lwd = 1.6, halo = TRUE, dx = 0, dy = 0) {
  for (k in contour_of(m)) {
    if (halo) lines(k$x + dx, k$y + dy, col = grDevices::adjustcolor("black", 0.55), lwd = lwd + 1.6)
    lines(k$x + dx, k$y + dy, col = col, lwd = lwd)
  }
}
dim_bg <- function(img, m, f = 0.45) {
  for (k in 1:3) { x <- img[, , k]; x[!m] <- x[!m] * f; img[, , k] <- x }
  img
}
box_px <- function(h, x1, y1, x2, y2, ...) rect(x1, h - y2, x2, h - y1, ...)
spearman <- function(x, y) stats::cor(x, y, method = "spearman", use = "complete.obs")

ex <- ft_example_photos()
res <- ft_example_results()
res <- res[match(ex$photo_id, res$photo_id), ]
gal_dir <- system.file("extdata", "gallery", package = "FlowerTrait")
gal <- utils::read.csv(file.path(gal_dir, "ATTRIBUTION.csv"), colClasses = c(photo_id = "character"))
v <- ft_validation
b <- v[v$role == "benchmark", ]
ct <- ft_catalogue
rf <- ft_reference

# ---------------------------------------------------------------- Figure 1: workflow
i1 <- which(ex$species == "Anemone coronaria")[1]
if (is.na(i1)) i1 <- which(ex$view_label == "face")[1]
r <- res[i1, ]
photo <- ft_read_image(ex$photo[i1]); crop <- ft_read_image(ex$crop[i1]); m <- ft_read_mask(ex$mask[i1])
s1 <- ft_symmetry(m, profile = TRUE); pr <- attr(s1, "profile")
tr1 <- ft_traits(crop, m)
fig("fig2_workflow.png", 7.2, 6.0, {
  layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 9, 9), 3, byrow = TRUE), heights = c(1, 1, 0.5),
         widths = c(1.15, 1, 1, 1.1))
  par(mar = c(0.5, 0.5, 2.2, 0.5))
  sc <- dim(photo)[2] / r$photo_w
  d <- draw_img(photo)
  box_px(d["h"], r$crop_x0 * sc, r$crop_y0 * sc, r$crop_x1 * sc, r$crop_y1 * sc, border = "white", lwd = 2.2, lty = 2)
  ptitle("Photo and crop", line = 0.3); plab("a", adj = 0.01)
  d <- draw_img(crop)
  box_px(d["h"], r$x1, r$y1, r$x2, r$y2, border = "#FFD400", lwd = 2.2)
  ptitle("Stage-2 box", line = 0.3); plab("b", adj = 0.01)
  d <- draw_img(dim_bg(crop, m))
  outline(m, "#7CF7FF", 1.8)
  ptitle("SAM 2.1 outline", line = 0.3); plab("c", adj = 0.01)
  par(mar = c(3.2, 6.4, 2.2, 2.2))
  p <- c("usable" = r$p_usable, "face view" = r$p_face_view, "open flower" = r$p_open_flower,
         "whole flower" = r$p_fully_visible, "sharp" = r$p_sharp)
  bp <- barplot(rev(p), horiz = TRUE, xlim = c(0, 1), col = ifelse(rev(p) >= 0.5, c_mid, "#B8C4D2"), border = NA,
                xlab = "Probability", axes = FALSE, cex.names = 0.95)
  axis(1, at = c(0, 0.5, 1)); abline(v = 0.5, lty = 2, col = ink2)
  text(rev(p), bp, sprintf("%.2f", rev(p)), pos = 4, cex = 0.75, col = ink, xpd = NA)
  ptitle("Photo filter", line = 0.3); plab("d", adj = -0.62)
  # radial zones
  par(mar = c(0.5, 0.5, 2.2, 0.5))
  d <- draw_img(crop)
  ang <- seq(0, 2 * pi, length.out = 361)
  rr <- c(pr$r, pr$r[1])
  for (f in c(0.3, 0.6)) {
    x <- (pr$cx + f * rr * cos(ang)) / pr$scale + 0.5
    y <- d["h"] - ((pr$cy + f * rr * sin(ang)) / pr$scale + 0.5)
    lines(x, y, col = "black", lwd = 3); lines(x, y, col = "white", lwd = 1.4, lty = 2)
  }
  outline(m, "white", 1.2)
  ptitle("Centre and petal zones", line = 0.3); plab("e", adj = 0.01)
  # radius profile
  par(mar = c(3.2, 3.3, 2.2, 0.6))
  th <- 0:359; rn <- pr$r / mean(pr$r)
  F <- stats::fft(pr$r) / length(pr$r); k <- s1$petal_n
  fit <- (Re(F[1]) + 2 * Re(F[k + 1] * exp(1i * k * th * pi / 180))) / mean(pr$r)
  plot(th, rn, type = "n", xlab = "Angle (degrees)", ylab = "Radius / mean", xaxt = "n", ylim = range(c(rn, fit)))
  gridlines(h = 1)
  axis(1, at = c(0, 90, 180, 270, 360))
  lines(th, fit, lty = 2, col = ink2, lwd = 1.2)
  lines(th, rn, col = c_hi, lwd = 1.8)
  ptitle(sprintf("Radius profile, harmonic %d", k), line = 0.3); plab("f", adj = -0.3)
  # symmetry overlay
  par(mar = c(0.5, 0.5, 2.2, 0.5))
  wm <- pr$mask
  hh <- nrow(wm); ww <- ncol(wm)
  x0 <- pr$cx + 0.5; y0 <- hh - (pr$cy + 0.5)
  plot.new(); plot.window(c(0, ww), c(0, hh), asp = 1)
  a <- 2 * pi / k
  for (cc in contour_of(wm)) {
    polygon(cc$x, cc$y, col = grDevices::adjustcolor(c_mid, 0.25), border = c_mid, lwd = 1.6)
    xr <- x0 + (cc$x - x0) * cos(a) - (cc$y - y0) * sin(a)
    yr <- y0 + (cc$x - x0) * sin(a) + (cc$y - y0) * cos(a)
    lines(xr, yr, col = c_rej, lwd = 1.4, lty = 2)
  }
  points(x0, y0, pch = 3, col = ink, cex = 0.8)
  text(ww / 2, -0.02 * hh, sprintf("sym_rotational %.2f   sym_bilateral %.2f", s1$sym_rotational, s1$sym_bilateral),
       cex = 0.72, col = ink, xpd = NA)
  ptitle(sprintf("Rotated by %.0f degrees", 360 / k), line = 0.3); plab("g", adj = 0.01)
  # colours
  par(mar = c(0.5, 0.8, 2.2, 0.5))
  plot.new(); plot.window(c(0, 10), c(0, 10))
  cen_L <- tr1$petal_L + tr1$centre_dL
  sw <- list(c("Petal periphery", lab_hex(tr1$petal_L, tr1$petal_a, tr1$petal_b)),
             c("Whole flower (mean)", lab_hex(tr1$L_mean, tr1$a_mean, tr1$b_mean)))
  for (j in seq_along(sw)) {
    y0 <- 9.6 - j * 3
    rect(0.2, y0, 3.2, y0 + 2.4, col = sw[[j]][2], border = "grey60")
    text(3.6, y0 + 1.2, sw[[j]][1], adj = 0, cex = 0.82, col = ink)
  }
  text(0.2, 2.2, sprintf("colour_name: %s", tr1$colour_name), adj = 0, cex = 0.82, col = ink)
  text(0.2, 0.9, sprintf("centre_dL: %.1f   centre_dE: %.1f", tr1$centre_dL, tr1$centre_dE), adj = 0, cex = 0.82,
       col = ink)
  ptitle("Colour traits", line = 0.3); plab("h", adj = 0.01)
  # pipeline
  par(mar = c(0.2, 0.4, 0.6, 0.4))
  plot.new(); plot.window(c(0, 100), c(0, 40))
  steps <- c("Photo, folder,\nspecies folders,\nGBIF, iNaturalist", "Grounding DINO\n'flower' on the\nwhole photo",
             "Square crop at\nnative resolution", "Grounding DINO\nagain in\nthe crop", "SAM 2.1 outline\nand stability",
             "Outline QC and\nphoto filter", "Decision rule", "Traits and\nreliability\ncatalogue")
  fill <- c("#EEF2F6", rep("#DDE7F2", 4), "#FBE8CF", "#FBE8CF", "#DCEFE4")
  xs <- seq(6.2, 93.8, length.out = length(steps))
  for (j in seq_along(steps)) {
    rect(xs[j] - 5.7, 15, xs[j] + 5.7, 39, col = fill[j], border = NA)
    text(xs[j], 27, steps[j], cex = 0.66, col = ink)
    if (j < length(steps)) arrows(xs[j] + 5.8, 27, xs[j + 1] - 5.8, 27, length = 0.05, col = ink2, lwd = 1.2)
  }
  rect(xs[2] - 5.7, 11.5, xs[6] + 5.7, 12.5, col = c_mid, border = NA)
  text(mean(xs[2:6]), 6.5, "Python back end (ft_process), run as a separate process", cex = 0.68, col = c_mid)
  rect(xs[7] - 5.7, 11.5, xs[8] + 5.7, 12.5, col = c_keep, border = NA)
  text(mean(xs[7:8]), 6.5, "R (ft_filter, ft_traits, ft_symmetry)", cex = 0.68, col = c_keep)
  plab("i", line = -0.6, adj = 0)
})

# ---------------------------------------------------------------- Figure 2: validation photographs
fig("fig1_design.png", 7.2, 7.1, {
  par(oma = c(0, 0, 0.8, 0))
  layout(matrix(c(rep(1, 6), 2:19), 4, byrow = TRUE), heights = c(2.3, 1, 1, 1))
  par(mar = c(0.2, 0.2, 1.8, 0.2))
  if (requireNamespace("maps", quietly = TRUE)) {
    maps::map("world", fill = TRUE, col = "#EDEFF2", border = "#C9CED6", lwd = 0.3, ylim = c(-56, 75),
              xlim = c(-170, 180), mar = c(0.2, 0.2, 1.8, 0.2))
  } else {
    plot(NA, xlim = c(-170, 180), ylim = c(-56, 75), asp = 1, axes = FALSE, xlab = "", ylab = "")
    box(col = grid_col)
  }
  pts <- v[!is.na(v$decimalLatitude), ]
  pts <- pts[!duplicated(pts$occurrenceKey), ]
  points(pts$decimalLongitude, pts$decimalLatitude, pch = 21, cex = 0.8, lwd = 0.35, col = "white",
         bg = grDevices::adjustcolor(region_col[pts$region], 0.9))
  nsp <- tapply(v$species, v$region, function(z) length(unique(z)))[names(region_col)]
  legend("bottomleft", legend = sprintf("%s: %d species", region_lab[names(region_col)], nsp), pt.bg = region_col,
         pch = 21, col = "white", bty = "n", cex = 0.78, inset = c(0.005, 0.02), text.col = ink)
  ptitle(sprintf("%d photographs from %d observations of %d species in %d countries", nrow(v),
                 length(unique(v$occurrenceKey)), length(unique(v$species)), length(unique(v$country))))
  plab("a", adj = 0)
  par(mar = c(0.15, 0.15, 1.25, 0.15))
  g <- gal[gal$note == "region", ]
  g <- g[order(match(g$region, names(region_col))), ]
  for (j in seq_len(min(18, nrow(g)))) {
    im <- ft_read_image(file.path(gal_dir, paste0(g$photo_id[j], ".jpg")))
    mm <- ft_read_mask(file.path(gal_dir, paste0(g$photo_id[j], ".png")))
    dd <- draw_img(im, border = region_col[g$region[j]], lwd = 3, square = TRUE)
    outline(mm, "white", 1.1, halo = TRUE, dx = dd["x0"], dy = dd["y0"])
    mtext(g$species[j], side = 3, line = 0.15, cex = 0.58, font = 3, col = ink)
    if (j == 1) plab("b", line = 0.15, adj = 0)
  }
})

# ---------------------------------------------------------------- Figure 3: outlines against manual segmentations
if (NROW(ft_benchmark$segmentation)) {
seg <- ft_benchmark$segmentation
si <- ft_benchmark$segmentation_images
cfg_lab <- function(d, ts, s) {
  dn <- c(gdino_b = "GDINO-B", gdino_t = "GDINO-T", owlv2_b = "OWLv2-B", none = "No detector")[d]
  sn <- c("sam2.1_b" = "SAM 2.1-B", "sam2.1_s" = "SAM 2.1-S", "sam2.1_t" = "SAM 2.1-T", grabcut = "GrabCut")[s]
  paste0(dn, ifelse(d == "none", "", ifelse(ts, " (2 stages)", " (1 stage)")), " + ", sn)
}
seg$label <- cfg_lab(seg$detector, seg$two_stage, seg$segmenter)
si$label <- cfg_lab(si$detector, si$two_stage, si$segmenter)
def_lab <- cfg_lab("gdino_b", TRUE, "sam2.1_b")
fig("fig3_outlines.png", 7.2, 5.4, {
  layout(matrix(c(1, 2, 3, 3), 2, byrow = TRUE), heights = c(1.15, 1), widths = c(1.35, 1))
  par(mar = c(3.4, 12.5, 2, 0.8))
  o <- seg$label[order(seg$iou_mean)]
  isdef <- o == def_lab
  plot(0, type = "n", xlim = c(0, 1), ylim = c(0.5, length(o) + 0.5), yaxt = "n", xlab = "IoU with manual outline",
       ylab = "")
  gridlines(v = seq(0, 1, 0.2))
  for (j in seq_along(o)) {
    x <- si$iou[si$label == o[j]]
    q <- stats::quantile(x, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
    segments(q[1], j, q[5], j, col = ifelse(isdef[j], c_hi, ink2), lwd = 1.2)
    rect(q[2], j - 0.28, q[4], j + 0.28, col = ifelse(isdef[j], "#F7DDB3", c_light), border = ifelse(isdef[j], c_hi, c_mid))
    segments(q[3], j - 0.28, q[3], j + 0.28, col = ink, lwd = 1.6)
    points(mean(x), j, pch = 18, col = ink, cex = 0.9)
  }
  axis(2, at = seq_along(o)[!isdef], labels = o[!isdef], cex.axis = 0.78, tick = FALSE, col.axis = ink)
  axis(2, at = seq_along(o)[isdef], labels = o[isdef], cex.axis = 0.78, tick = FALSE, col.axis = ink, font.axis = 2)
  ptitle(sprintf("IoU per setting (%d photographs)", length(unique(si$image))))
  plab("a", adj = -0.68)
  par(mar = c(3.4, 3.4, 2, 0.8))
  plot(seg$sec, seg$iou_mean, log = "x", type = "n", xlab = "Seconds per photograph (CPU)",
       ylab = "Mean IoU", ylim = c(min(seg$iou_mean) - 0.05, 1), xlim = c(0.25, 60))
  gridlines(h = seq(0, 1, 0.1))
  isd <- seg$detector == "gdino_b"
  points(seg$sec, seg$iou_mean, pch = 21, cex = 1.6, col = "white",
         bg = ifelse(seg$label == def_lab, c_hi, ifelse(isd, c_mid, ifelse(seg$segmenter == "grabcut", "#8E5DB5", c_rej))))
  nd <- !isd
  text(seg$sec[nd], seg$iou_mean[nd], sub(" \\+ ", "\n+ ", seg$label[nd]), cex = 0.55,
       pos = ifelse(seg$sec[nd] > 10, 2, 4), col = ink2, xpd = NA)
  text(0.3, 0.97, "Grounding DINO-B boxes with SAM 2.1-B/S/T\nor GrabCut, one or two stages (orange: default)",
       cex = 0.55, col = ink2, adj = c(0, 0.5))
  ptitle("Accuracy and time"); plab("b", adj = -0.2)
  par(mar = c(6.2, 3.4, 1.6, 0.8))
  dd <- si[si$label == def_lab, ]
  cls <- names(sort(tapply(dd$iou, dd$cls, stats::median)))
  f <- factor(dd$cls, levels = cls)
  plot(0, type = "n", xlim = c(0.5, length(cls) + 0.5), ylim = c(0, 1), xaxt = "n", xlab = "", ylab = "IoU (default)")
  gridlines(h = seq(0, 1, 0.2))
  boxplot(dd$iou ~ f, add = TRUE, outline = FALSE, col = c_light, border = c_mid, xaxt = "n", yaxt = "n", boxwex = 0.6)
  set.seed(4)
  points(as.integer(f) + stats::runif(nrow(dd), -0.17, 0.17), dd$iou, pch = 16, cex = 0.45,
         col = grDevices::adjustcolor(ink, 0.55))
  axis(1, at = seq_along(cls), labels = cls, las = 2, cex.axis = 0.78, tick = FALSE, col.axis = ink)
  ptitle("Default pipeline by flower class"); plab("c", adj = -0.04)
})
}

# ---------------------------------------------------------------- Figure 4: photo filter and decision rules
dr <- ft_benchmark$decision_rules
pf <- ft_benchmark$photo_filter
rule_names <- c(none = "No rule", qc = "Outline QC", default = "Default", strict = "Strict", face_view = "Face view")
fig("fig4_filter.png", 7.2, 6.6, {
  layout(rbind(c(1, 1, 1, 2, 2, 2), c(3, 3, 3, 4, 4, 4), 5:10), heights = c(1, 1, 0.8))
  par(mar = c(3.2, 6.6, 2, 1.2))
  kept <- dr$kept[match(names(rule_names), dr$rule)] * 100
  bp <- barplot(rev(kept), horiz = TRUE, names.arg = rev(rule_names), col = c_mid, border = NA, xlim = c(0, 100),
                xlab = "Benchmark photographs kept (%)", axes = FALSE)
  axis(1, at = seq(0, 100, 25))
  text(rev(kept), bp, sprintf("%.0f", rev(kept)), pos = 4, cex = 0.78, col = ink, xpd = NA)
  ptitle("Share kept by each rule"); plab("a", adj = -0.3)
  par(mar = c(3.2, 6.6, 2, 1.2))
  mat <- t(as.matrix(dr[match(names(rule_names), dr$rule), c("good", "minor", "several", "wrong")])) * 100
  cols <- c("#2E7D5B", "#A8D5BA", "#E8A33D", "#B5443B")
  barplot(mat[, ncol(mat):1], horiz = TRUE, names.arg = rev(rule_names), col = cols, border = "white", axes = FALSE,
          xlab = "Outlines among kept photographs (%)")
  axis(1, at = seq(0, 100, 25))
  legend("top", c("good", "minor error", "several flowers", "wrong"), fill = cols, border = NA, bty = "n", cex = 0.7,
         horiz = TRUE, inset = c(0, -0.13), xpd = NA, text.col = ink)
  plab("b", adj = -0.3)
  par(mar = c(3.2, 6.6, 2, 1.2))
  qs <- c("usable", "face_view", "open_flower", "fully_visible", "sharp")
  ql <- c("Usable", "Face view", "Open flower", "Whole flower", "Sharp")
  get <- function(q, f) { z <- pf$auc[pf$question == q & f]; if (length(z)) z[1] else NA }
  m3 <- rbind(sapply(qs, function(q) get(q, pf$used)), sapply(qs, function(q) get(q, pf$features == "QC+shape")),
              sapply(qs, function(q) get(q, pf$features == "zero-shot CLIP")))
  barplot(m3[, 5:1], beside = TRUE, horiz = TRUE, names.arg = rev(ql), col = c(c_mid, "#8FA9C8", "#D2DCE8"),
          border = NA, xlim = c(0.4, 1), xpd = FALSE, xlab = "Cross-validated AUC", axes = FALSE)
  axis(1, at = seq(0.4, 1, 0.1))
  legend("top", c("shipped head", "quality features", "zero-shot CLIP"), fill = c(c_mid, "#8FA9C8", "#D2DCE8"),
         border = NA, bty = "n", cex = 0.68, text.col = ink, horiz = TRUE, inset = c(0, -0.13), xpd = NA)
  plab("c", adj = -0.3)
  par(mar = c(3.2, 4.2, 2, 1.2))
  neg <- v[v$role == "negative", ]
  an <- data.frame(p = c(b$p_open_flower, neg$p_open_flower),
                   g = factor(rep(c("flowering", "not flowering"), c(nrow(b), nrow(neg))), c("flowering", "not flowering")))
  plot(0, type = "n", xlim = c(0.5, 2.5), ylim = c(0, 1), xaxt = "n", xlab = "iNaturalist annotation", ylab = "p_open_flower")
  gridlines(h = seq(0, 1, 0.25))
  boxplot(p ~ g, data = an, add = TRUE, col = c("#DCEFE4", "#F4D9D6"), border = c(c_keep, c_rej), outline = FALSE,
          xaxt = "n", yaxt = "n", boxwex = 0.5)
  set.seed(5)
  points(as.integer(an$g) + stats::runif(nrow(an), -0.2, 0.2), an$p, pch = 16, cex = 0.4,
         col = grDevices::adjustcolor(ink, 0.45))
  axis(1, at = 1:2, labels = sprintf("%s (%d)", levels(an$g), tabulate(an$g)), tick = FALSE, col.axis = ink)
  abline(h = 0.5, lty = 2, col = ink2)
  a1 <- ft_benchmark$annotations
  ptitle(sprintf("Independent labels: AUC = %.2f", a1$auc[a1$measure == "p_open_flower"])); plab("d", adj = -0.2)
  # example photographs with the reason of rejection
  g <- gal[gal$note != "region", ]
  g <- g[match(c("kept", "bud", "side", "blurred", "hidden", "no flower"), g$note), ]
  g <- g[!is.na(g$photo_id), ]
  vv <- v[match(g$photo_id, v$photo_id), ]
  par(mar = c(0.2, 0.2, 2.4, 0.2))
  for (j in seq_len(nrow(g))) {
    im <- ft_read_image(file.path(gal_dir, paste0(g$photo_id[j], ".jpg")))
    mm <- ft_read_mask(file.path(gal_dir, paste0(g$photo_id[j], ".png")))
    k <- ft_filter(vv[j, ], if (g$note[j] == "side") "face_view" else "default")
    dd <- draw_img(im, border = if (k$keep) c_keep else c_rej, lwd = 3, square = TRUE)
    outline(mm, "white", 1, dx = dd["x0"], dy = dd["y0"])
    lbl <- switch(g$note[j], kept = sprintf("kept, p_usable %.2f", vv$p_usable[j]),
                  side = sprintf("side, p_face %.2f", vv$p_face_view[j]),
                  sprintf("%s, p_usable %.2f", g$note[j], vv$p_usable[j]))
    mtext(lbl, side = 3, line = 1.15, cex = 0.56, col = ink)
    mtext(g$species[j], side = 3, line = 0.25, cex = 0.56, font = 3, col = ink2)
    if (j == 1) plab("e", line = 1.1, adj = 0)
  }
})

# ---------------------------------------------------------------- Figure 5: trait reliability
num <- ct[ct$type == "numeric", ]
fig("fig5_reliability.png", 7.2, 6.6, {
  layout(matrix(1:4, 2, byrow = TRUE), widths = c(1, 1.1))
  par(mar = c(4.6, 3.6, 2, 0.8))
  sets <- c(all = "icc_all", qc = "icc_qc", default = "icc_default", strict = "icc_strict", face = "icc_face")
  med <- sapply(sets, function(s) tapply(num[[s]], num$family, stats::median, na.rm = TRUE))
  npair <- sapply(c("n_all", "n_qc", "n_default", "n_strict", "n_face"), function(s) stats::median(num[[s]], na.rm = TRUE))
  plot(0, type = "n", xlim = c(0.7, 5.3), ylim = c(min(0.3, med, na.rm = TRUE), 1), xaxt = "n", xlab = "",
       ylab = "Median photo-to-photo ICC")
  gridlines(h = seq(0, 1, 0.1))
  for (fm in rownames(med)) {
    lines(1:5, med[fm, ], col = fam_col[fm], lwd = 2)
    points(1:5, med[fm, ], pch = 21, bg = fam_col[fm], col = "white", cex = 1.4)
  }
  mtext(c("all", "QC +\nstability", "default", "strict", "face\nview"), side = 1, at = 1:5, line = 1.3, cex = 0.72,
        col = ink2)
  mtext(paste0("n = ", npair), side = 1, at = 1:5, line = 3.1, cex = 0.6, col = ink2)
  legend("bottomright", sprintf("%s (%d)", rownames(med), table(num$family)[rownames(med)]), col = fam_col[rownames(med)],
         pch = 16, lwd = 2, bty = "n", cex = 0.72, text.col = ink)
  ptitle("Repeatability by trait family and rule"); plab("a", adj = -0.15)
  par(mar = c(4.6, 3.6, 2, 0.8))
  plot(num$icc_strict, num$inv_photometric_min, type = "n", xlim = c(min(0, num$icc_strict, na.rm = TRUE), 1),
       ylim = c(min(0, num$inv_photometric_min, na.rm = TRUE), 1), xlab = "Repeatability (ICC, strict rule)",
       ylab = "Smallest photometric invariance")
  gridlines(h = seq(-1, 1, 0.2), v = seq(-1, 1, 0.2))
  abline(v = c(0.4, 0.6), h = c(0.7, 0.8), lty = c(3, 2, 3, 2), col = ink2)
  points(num$icc_strict, num$inv_photometric_min, pch = 21, bg = verdict_col[num$verdict], col = "white", cex = 1.35)
  core <- num$trait %in% FlowerTrait:::.ft_core_traits
  points(num$icc_strict[core], num$inv_photometric_min[core], pch = 1, cex = 1.55, col = ink, lwd = 0.8)
  legend("bottomleft", c(sprintf("%s (%d)", names(verdict_col), table(factor(num$verdict, names(verdict_col)))),
                         "core trait"), pt.bg = c(verdict_col, NA), pch = c(21, 21, 21, 1), col = c(rep("white", 3), ink),
         bty = "n", cex = 0.72, text.col = ink)
  ptitle("Catalogue verdicts"); plab("b", adj = -0.15)
  par(mar = c(3.8, 3.6, 2, 6.6))
  sel <- intersect(c("L_mean", "centre_dE", "sym_rotational", "petal_n", "colour_shannon", "edge_density",
                     "glcm_contrast", "gran_D16", "land_n_patches"), num$trait)
  cl <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#CC79A7", "#999933", "#882255")[seq_along(sel)]
  ys <- as.matrix(num[match(sel, num$trait), paste0("inv_D", c(32, 64, 128, 256))])
  plot(0, type = "n", xlim = c(1, 4), ylim = c(min(0, ys, na.rm = TRUE), 1), xaxt = "n",
       xlab = "Flower diameter after reduction (px)", ylab = "ICC with full resolution")
  gridlines(h = seq(-1, 1, 0.2))
  axis(1, at = 1:4, labels = c(32, 64, 128, 256))
  abline(h = 0.8, lty = 2, col = ink2)
  for (j in seq_along(sel)) {
    lines(1:4, ys[j, ], col = cl[j], lwd = 1.8)
    points(1:4, ys[j, ], pch = 21, bg = cl[j], col = "white", cex = 1.2)
  }
  legend("topleft", inset = c(1.01, 0), sel, col = cl, pch = 16, lwd = 1.8, bty = "n", cex = 0.66, xpd = NA,
         text.col = ink)
  ptitle("Resolution needed"); plab("c", adj = -0.15)
  # colour correction: change in repeatability
  wb <- ft_benchmark$white_balance
  if (NROW(wb)) {
    par(mar = c(3.8, 6.4, 2, 0.8))
    meth <- c("gray_world", "white_patch", "shades_of_gray", "gray_pixels")
    mcol <- c(gray_world = "#B5443B", white_patch = "#E8A33D", shades_of_gray = "#3E6A9E", gray_pixels = "#2E7D5B")
    trs <- c("L_mean", "C_mean", "a_mean", "b_mean", "hue_deg", "hue_conc", "petal_L", "centre_dE")
    plot(0, type = "n", xlim = range(c(-0.5, 0.1, wb$lower[wb$method %in% meth & wb$trait %in% trs]), na.rm = TRUE),
         ylim = c(0.5, length(trs) + 0.5), yaxt = "n", xlab = "Change in ICC after correction", ylab = "")
    gridlines(v = seq(-1, 1, 0.1))
    abline(v = 0, col = ink2)
    for (k in seq_along(meth)) {
      d <- wb[wb$method == meth[k], ]
      d <- d[match(rev(trs), d$trait), ]
      yy <- seq_along(trs) + (k - 2.5) * 0.17
      segments(d$lower, yy, d$upper, yy, col = mcol[meth[k]], lwd = 1.3)
      points(d$diff, yy, pch = 21, bg = mcol[meth[k]], col = "white", cex = 1.1)
    }
    base <- wb[wb$method == "none", ]
    axis(2, at = seq_along(trs), labels = sprintf("%s (%.2f)", rev(trs), base$icc[match(rev(trs), base$trait)]),
         tick = FALSE, cex.axis = 0.8, col.axis = ink)
    legend("topleft", gsub("_", " ", meth), pt.bg = mcol, pch = 21, col = "white", bty = "n", cex = 0.66,
           text.col = ink)
    ptitle(sprintf("Colour correction (%d pairs)", wb$n_pairs[1])); plab("d", adj = -0.6)
  }
})

# ---------------------------------------------------------------- Figure 6: photographs against reflectance spectra
r6 <- rf[!is.na(rf$photo_L), ]
fig("fig6_spectra.png", 7.2, 7.0, {
  layout(matrix(c(1, 2, 3, 4, 4, 5), 2, byrow = TRUE), heights = c(1, 1.2))
  par(mar = c(3.6, 3.6, 2, 0.8))
  for (q in list(c("L", "L*", "a"), c("a", "a*", "b"), c("b", "b*", "c"))) {
    x <- r6[[paste0("ref_", q[1])]]; y <- r6[[paste0("photo_", q[1])]]
    lim <- range(c(x, y), na.rm = TRUE) + c(-3, 3)
    plot(x, y, xlim = lim, ylim = lim, type = "n", xlab = paste(q[2], "from spectra"), ylab = paste(q[2], "from photographs"))
    gridlines(h = pretty(lim), v = pretty(lim))
    abline(0, 1, lty = 2, col = ink2)
    points(x, y, pch = 21, cex = 1.75, lwd = 2, col = region_col[r6$region], bg = lab_hex(r6$ref_L, r6$ref_a, r6$ref_b))
    legend("topleft", sprintf("\u03c1 = %.2f", spearman(x, y)), bty = "n", cex = 0.9, text.col = ink)
    plab(q[3], adj = -0.22)
  }
  legend("bottomright", region_lab[names(region_col)], pch = 21, pt.lwd = 2, col = region_col, pt.bg = "white",
         bty = "n", cex = 0.62, text.col = ink, title = "Region (outline)", title.col = ink2)
  par(mar = c(4.6, 5.6, 2.4, 1))
  lev <- name_lev[name_lev %in% c(r6$ref_name, r6$photo_name)]
  tab <- table(factor(r6$ref_name, lev), factor(r6$photo_name, lev))
  nl <- length(lev)
  plot(0, type = "n", xlim = c(0.5, nl + 0.5), ylim = c(0.5, nl + 0.5), axes = FALSE, xlab = "", ylab = "", asp = 1)
  for (ii in seq_len(nl)) for (jj in seq_len(nl)) {
    z <- tab[ii, jj]
    rect(jj - 0.5, ii - 0.5, jj + 0.5, ii + 0.5, border = "white", lwd = 1.5,
         col = if (z == 0) "#F3F5F7" else if (ii == jj) grDevices::adjustcolor(c_keep, 0.25 + 0.75 * z / max(tab))
               else grDevices::adjustcolor(c_rej, 0.2 + 0.6 * z / max(tab)))
    if (z > 0) text(jj, ii, z, cex = 0.85, col = ink)
  }
  for (ii in seq_len(nl)) {
    rect(-0.15, ii - 0.32, 0.35, ii + 0.32, col = name_col[lev[ii]], border = "grey55", xpd = NA)
    rect(ii - 0.32, -0.15, ii + 0.32, 0.35, col = name_col[lev[ii]], border = "grey55", xpd = NA)
  }
  axis(2, at = seq_len(nl), labels = lev, tick = FALSE, line = 0.2, col.axis = ink)
  axis(1, at = seq_len(nl), labels = lev, las = 2, tick = FALSE, line = 0.2, col.axis = ink)
  mtext("Colour name from photographs", side = 1, line = 3.6, cex = 0.8, col = ink)
  mtext("Colour name from spectra", side = 2, line = 4.4, cex = 0.8, col = ink, las = 0)
  ptitle(sprintf("Same name for %d of %d species", sum(diag(tab)), sum(tab)), line = 0.8)
  plab("d", adj = -0.05, line = 0.8)
  # paired swatches
  par(mar = c(0.5, 0.5, 2.4, 0.5))
  s6 <- r6[order(ifelse(r6$ref_C < 15, 400 + r6$ref_L, r6$ref_h)), ]
  s6 <- s6[unique(round(seq(1, nrow(s6), length.out = min(24, nrow(s6))))), ]
  n6 <- nrow(s6)
  plot.new(); plot.window(c(0, 10), c(0, n6 + 1))
  text(1.0, n6 + 0.9, "spectra", cex = 0.66, col = ink2); text(2.2, n6 + 0.9, "photos", cex = 0.66, col = ink2)
  for (j in seq_len(n6)) {
    y <- n6 - j + 0.5
    rect(0.45, y - 0.38, 1.55, y + 0.38, col = lab_hex(s6$ref_L[j], s6$ref_a[j], s6$ref_b[j]), border = "grey60")
    rect(1.65, y - 0.38, 2.75, y + 0.38, col = lab_hex(s6$photo_L[j], s6$photo_a[j], s6$photo_b[j]), border = "grey60")
    text(2.95, y, s6$species[j], adj = 0, cex = 0.6, font = 3, col = ink)
    points(9.7, y, pch = 15, col = region_col[s6$region[j]], cex = 0.9)
  }
  plab("e", adj = 0, line = 0.8)
})

# ---------------------------------------------------------------- Figure 7: symmetry and petal number
face <- b[ft_filter(b, "face_view")$keep, ]
fig("fig7_symmetry.png", 7.2, 3.0, {
  layout(matrix(1:3, 1), widths = c(1, 0.85, 1))
  par(mar = c(3.6, 3.6, 2, 0.8))
  pc <- suppressWarnings(as.numeric(face$label_petals))
  okp <- !is.na(pc) & face$outline_rating %in% c("good", "minor error")
  tt <- table(factor(pc[okp], 3:8), factor(pmin(face$petal_n[okp], 13), 3:13))
  plot(0, type = "n", xlim = c(2.5, 8.5), ylim = c(2.5, 13.5), xlab = "Petals counted", ylab = "petal_n (outline)",
       yaxt = "n")
  axis(2, at = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, "\u226513"))
  gridlines(h = 3:13, v = 3:8)
  abline(0, 1, lty = 2, col = ink2)
  w <- which(tt > 0, arr.ind = TRUE)
  xv <- as.numeric(rownames(tt))[w[, 1]]; yv <- as.numeric(colnames(tt))[w[, 2]]
  symbols(xv, yv, circles = sqrt(tt[w]) * 0.13, inches = FALSE, add = TRUE, fg = "white",
          bg = ifelse(xv == yv, grDevices::adjustcolor(c_keep, 0.8), grDevices::adjustcolor(c_mid, 0.6)))
  text(xv, yv, tt[w], cex = 0.62, col = "white")
  legend("topleft", sprintf("same number: %.0f%% of %d", 100 * mean(face$petal_n[okp] == pc[okp]), sum(okp)),
         bty = "n", cex = 0.78, text.col = ink)
  ptitle("Petal number (face views)"); plab("a", adj = -0.22)
  par(mar = c(3.6, 3.6, 2, 0.8))
  rs <- rf[rf$symmetry %in% c("radial", "bilateral") & !is.na(rf$photo_sym_rot) & rf$n_face > 0, ]
  rs$symmetry <- factor(rs$symmetry, c("radial", "bilateral"))
  plot(0, type = "n", xlim = c(0.5, 2.5), ylim = range(c(0.3, 1, rs$photo_sym_rot)), xaxt = "n", xlab = "FReD symmetry",
       ylab = "sym_rotational (species median)")
  gridlines(h = seq(0, 1, 0.1))
  boxplot(photo_sym_rot ~ symmetry, data = rs, add = TRUE, col = c("#E7DDF2", "#F6E7D6"), border = c("#8E5DB5", c_hi),
          outline = FALSE, xaxt = "n", yaxt = "n", boxwex = 0.5)
  set.seed(7)
  points(as.integer(rs$symmetry) + stats::runif(nrow(rs), -0.15, 0.15), rs$photo_sym_rot, pch = 21, cex = 1.3,
         bg = lab_hex(rs$photo_L, rs$photo_a, rs$photo_b), col = ink2)
  axis(1, at = 1:2, labels = sprintf("%s (%d)", levels(rs$symmetry), tabulate(rs$symmetry)), tick = FALSE, col.axis = ink)
  ptitle("Against FReD"); plab("b", adj = -0.3)
  par(mar = c(3.6, 3.6, 2, 0.8))
  kf <- ft_filter(v[v$role != "negative", ], "face_view")
  kf <- kf[kf$keep, ]
  kf <- kf[!duplicated(kf$occurrenceKey), ]
  plot(kf$sym_rotational, kf$sym_bilateral, type = "n", xlim = c(0.2, 1), ylim = c(0.4, 1), xlab = "sym_rotational",
       ylab = "sym_bilateral")
  gridlines(h = seq(0, 1, 0.1), v = seq(0, 1, 0.1))
  symc <- rf$symmetry[match(kf$ref_species, rf$ref_species)]
  bgc <- ifelse(symc %in% "bilateral", c_hi, ifelse(symc %in% "radial", "#8E5DB5", "#C9CED6"))
  points(kf$sym_rotational, kf$sym_bilateral, pch = 21, bg = grDevices::adjustcolor(bgc, 0.85), col = "white", cex = 1.1)
  legend("bottomright", c("radial (FReD)", "zygomorphic (FReD)", "no FReD record"), pt.bg = c("#8E5DB5", c_hi, "#C9CED6"),
         pch = 21, col = "white", bty = "n", cex = 0.7, text.col = ink)
  ptitle(sprintf("%d face-view photographs", nrow(kf))); plab("c", adj = -0.22)
})

# ---------------------------------------------------------------- Figure 8: worked example
fig("fig8_example.png", 7.2, 7.4, {
  layout(rbind(1:4, 5:8, rep(9, 4)), heights = c(1, 1, 1.75))
  par(mar = c(0.3, 0.3, 2.6, 0.3))
  for (j in seq_len(nrow(ex))) {
    im <- ft_read_image(ex$crop[j]); mm <- ft_read_mask(ex$mask[j])
    rr <- res[j, ]
    dd <- draw_img(dim_bg(im, mm, 0.5), border = if (rr$keep) c_keep else c_rej, lwd = 3, square = TRUE)
    outline(mm, "#7CF7FF", 1.3, dx = dd["x0"], dy = dd["y0"])
    mtext(ex$species[j], side = 3, line = 1.35, cex = 0.62, font = 3, col = ink)
    mtext(if (rr$keep) sprintf("kept: %s, %s", rr$reason, rr$colour_name) else rr$reason, side = 3, line = 0.4,
          cex = 0.58, col = if (rr$keep) c_keep else c_rej)
    if (j == 1) plab("a", line = 1.3, adj = 0)
  }
  par(mar = c(1, 12, 2.6, 6))
  kept <- ft_filter(v[v$role != "negative", ], "default")
  kept <- kept[kept$keep, ]
  kept <- kept[!duplicated(kept$occurrenceKey), ]
  kept$group <- paste0(kept$species, " (", region_lab[kept$region], ")")
  top <- names(sort(table(kept$group), decreasing = TRUE))
  top <- top[seq_len(min(24, length(top)))]
  kk <- kept[kept$group %in% top, ]
  hue_key <- sapply(top, function(gr) {
    z <- kk[kk$group == gr, ]
    a <- stats::median(z$petal_a); bb <- stats::median(z$petal_b)
    if (sqrt(a^2 + bb^2) < 15) 400 + stats::median(z$petal_L) else (atan2(bb, a) * 180 / pi) %% 360
  })
  kk$group <- factor(kk$group, levels = top[order(hue_key)])
  ft_plot_colours(kk, group = "group", max_per_group = 9)
  plab("b", adj = 0, line = 0.8)
})
# ---------------------------------------------------------------- Figure S1: augmentation
aug <- ft_benchmark$augmentation
if (NROW(aug)) {
  fig("figS1_augmentation.png", 7.2, 5.6, {
    layout(rbind(1:6, rep(7, 6)), heights = c(0.62, 1.6))
    j <- which(ex$species == "Gorteria diffusa")[1]
    if (is.na(j)) j <- 1
    im <- ft_read_image(ex$crop[j]); mm <- ft_read_mask(ex$mask[j])
    s <- 300 / max(dim(im)[1:2])
    if (s < 1) {
      h <- round(dim(im)[1] * s); w <- round(dim(im)[2] * s)
      im <- FlowerTrait:::.ft_resize_image(im, h, w)
      mm <- FlowerTrait:::.ft_area_resize(mm * 1, h, w) > 0.5
    }
    vars <- list(list("original", NULL, NULL), list("quarter turn", "rotate90", 1L), list("mirror", "flip", "horizontal"),
                 list("saturation x 1.25", "saturation", 1.25), list("hue + 0.05", "hue", 0.05),
                 list("white balance", "white_balance", c(1.2, 0.8)))
    par(mar = c(0.3, 0.3, 1.6, 0.3))
    for (k in seq_along(vars)) {
      a <- if (is.null(vars[[k]][[2]])) list(image = im, mask = mm) else ft_augment(im, mm, vars[[k]][[2]], vars[[k]][[3]])
      draw_img(a$image)
      outline(a$mask, "white", 1)
      ptitle(vars[[k]][[1]], line = 0.25, cex = 0.65)
      if (k == 1) plab("a", line = 0.2, adj = 0)
    }
    par(mar = c(4.2, 8.5, 1.6, 1))
    types <- c("rotate90", "flip", "rotate", "brightness", "contrast", "saturation", "hue", "white_balance", "exposure",
               "jpeg", "blur")
    tl <- c("quarter turn", "mirror", "rotation", "brightness", "contrast", "saturation", "hue", "white balance",
            "exposure", "JPEG", "blur")
    trs <- c("L_mean", "C_mean", "hue_deg", "petal_L", "centre_dE", "dark_frac_otsu", "sym_rotational", "petal_n")
    m <- sapply(trs, function(t) aug$median_rel_change[match(paste(types, t), paste(aug$type, aug$trait))])
    rownames(m) <- tl
    cols <- c("#2F6DB5", "#5B9BD5", "#E8A33D", "#B5443B", "#8E5DB5", "#555555", "#2E8B57", "#9BC79B")
    barplot(t(m[nrow(m):1, ]), beside = TRUE, horiz = TRUE, col = cols, border = NA, las = 1, xlim = c(0, max(m, 0.3, na.rm = TRUE) * 1.05),
            xlab = "Median change (between-photograph SD)", cex.names = 0.85)
    abline(v = 0.25, lty = 2, col = ink2)
    legend("topright", trs, fill = cols, border = NA, bty = "n", cex = 0.7, ncol = 2, text.col = ink)
    plab("b", line = 0.2, adj = -0.12)
  })
}
message("Figures written to ", normalizePath(out))
