#' Convert sRGB values to CIELAB
#'
#' Removes the sRGB gamma, then converts linear RGB to CIE XYZ and CIELAB with a
#' D65 white point, using the constants of 'OpenCV' (`COLOR_LRGB2Lab`), so that
#' the values match the 'Python' back end.
#'
#' @param img RGB array (height x width x 3, values 0-255), a file path, or a
#'   three-column matrix of RGB values.
#' @return An array (or matrix) of the same shape with `L*` (0-100), `a*` and `b*`.
#' @seealso [ft_colour_name()]
#' @examples
#' ft_rgb_to_lab(rbind(white = c(255, 255, 255), red = c(220, 30, 30),
#'                     violet = c(140, 90, 200)))
#' @export
ft_rgb_to_lab <- function(img) {
  if (is.matrix(img) && ncol(img) == 3L) {
    lab <- .ft_lab(.ft_lin(img[, 1]), .ft_lin(img[, 2]), .ft_lin(img[, 3]))
    out <- cbind(L = lab$L, a = lab$a, b = lab$b)
    rownames(out) <- rownames(img)
    return(out)
  }
  img <- .ft_as_image(img)
  lab <- .ft_lab(.ft_lin(img[, , 1]), .ft_lin(img[, , 2]), .ft_lin(img[, , 3]))
  array(c(lab$L, lab$a, lab$b), dim(img))
}

#' Core flower traits and outline quality from a photo and its outline
#'
#' Computes, natively in R, the outline quality metrics and the core traits of
#' the back end for a flower crop and its outline mask: colour traits of the
#' whole flower, the colour of the centre and of the petal periphery, and the
#' symmetry, petal-number and shape traits of [ft_symmetry()]. Formulas and
#' constants follow the 'Python' back end (linear sRGB, 'OpenCV' CIELAB, 'OpenCV'
#' Otsu threshold and Laplacian, the same working resolution and radius
#' function), so values agree with [ft_process()] to within rounding of the image
#' decoder. Granularity, texture, colour classes and the other extended traits
#' need the back end ([ft_extended_traits()]).
#'
#' The centre and periphery of a flower are defined by the relative radius of
#' each pixel: its distance from the centroid of the outline divided by the
#' radius of the outline in the same direction. Pixels with a relative radius
#' below 0.3 form the centre and those above 0.6 the petal periphery; the colour
#' name of the flower ([ft_colour_name()]) is that of the periphery.
#'
#' @param img Crop as an RGB array (values 0-255) or a path to a JPEG/PNG
#'   file. A character vector of several paths processes several photos.
#' @param mask Logical outline matrix of the same size, or a path (or paths)
#'   to PNG masks.
#' @param min_px Minimum outline size (pixels) for traits to be computed; the
#'   validated value is 600.
#' @param white_balance Optional colour correction of the photo before the
#'   colour traits are computed (quality metrics use the original photo); see
#'   [ft_white_balance()]. The validated default is `"none"`.
#' @return A data frame with one row per photo. Quality columns: `px` (outline
#'   pixels), `mask_frac`, `border_touch` (share of the crop border covered by
#'   the outline), `solidity`, `clip_hi` and `clip_lo` (share of flower pixels
#'   with the brightest channel >= 250 or <= 5), `sharp` (variance of the
#'   Laplacian inside the outline) and `mask_ok` (`px >= min_px` and
#'   `border_touch <= 0.10`; the back end also requires a detection). Trait
#'   columns: `D_px` (equivalent diameter), the colour traits `L_mean`, `L_sd`,
#'   `C_mean`, `a_mean`, `b_mean`, `hue_deg`, `hue_conc`, `vap_wp`,
#'   `dark_frac_otsu` and `L_entropy`, the radial traits `centre_dL`,
#'   `centre_dE`, `centre_frac`, `petal_L`, `petal_a`, `petal_b` and
#'   `colour_name`, and the geometric traits of [ft_symmetry()]. With file
#'   paths, a first column `photo_id` holds the file name without extension.
#' @seealso [ft_trait_info()] for definitions and reliability, [ft_symmetry()].
#' @examples
#' ex <- ft_example_photos()
#' ft_traits(ex$crop[1], ex$mask[1])
#'
#' # several photos at once
#' tr <- ft_traits(ex$crop, ex$mask)
#' tr[, c("photo_id", "L_mean", "C_mean", "colour_name", "petal_n", "sym_rotational")]
#'
#' # a synthetic yellow flower with five petals and a dark centre
#' img <- array(0, c(200, 200, 3)); img[, , 1] <- 60; img[, , 2] <- 110; img[, , 3] <- 50
#' yy <- row(img[, , 1]) - 100; xx <- col(img[, , 1]) - 100
#' flower <- sqrt(yy^2 + xx^2) < 60 + 25 * cos(5 * atan2(yy, xx))
#' centre <- sqrt(yy^2 + xx^2) < 18
#' img[, , 1][flower] <- 240; img[, , 2][flower] <- 200; img[, , 3][flower] <- 30
#' img[, , 1][centre] <- 90; img[, , 2][centre] <- 50; img[, , 3][centre] <- 20
#' ft_traits(img, flower)[, c("colour_name", "petal_n", "centre_dL", "sym_rotational")]
#' @export
ft_traits <- function(img, mask, min_px = 600,
                      white_balance = c("none", "gray_world", "white_patch", "shades_of_gray", "gray_pixels")) {
  white_balance <- match.arg(white_balance)
  if (is.character(img) && length(img) > 1L) {
    if (!is.character(mask) || length(mask) != length(img)) {
      stop("Give one mask path per image path.", call. = FALSE)
    }
    rows <- Map(function(i, m) ft_traits(i, m, min_px = min_px, white_balance = white_balance), img, mask)
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    return(out)
  }
  id <- if (is.character(img)) tools::file_path_sans_ext(basename(img)) else NULL
  img <- .ft_as_image(img)
  mask <- .ft_as_mask(mask)
  if (!identical(dim(img)[1:2], dim(mask))) {
    stop("`img` and `mask` must have the same height and width.", call. = FALSE)
  }
  q <- .ft_quality(img, mask, min_px)
  if (white_balance != "none") img <- ft_white_balance(img, white_balance)
  out <- c(q, .ft_core(img, mask, min_px))
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if (!is.null(id)) out <- cbind(photo_id = id, out, stringsAsFactors = FALSE)
  out
}

.ft_f32 <- function(v) {
  readBin(writeBin(as.double(v), raw(), size = 4), "double", size = 4, n = length(v))
}

# sRGB (0-255) -> linear RGB through a lookup table rounded to float32, as in
# the back end (which works in float32).
.ft_lin_lut <- local({
  v <- (0:255) / 255
  readBin(writeBin(ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4), raw(), size = 4),
          "double", size = 4, n = 256L)
})

.ft_lin <- function(x) {
  i <- as.integer(round(x))
  if (anyNA(i) || any(i < 0L | i > 255L)) stop("RGB values must lie in 0-255.", call. = FALSE)
  out <- .ft_lin_lut[i + 1L]
  dim(out) <- dim(x)
  out
}

.ft_lab <- function(R, G, B) {
  X <- (0.412453 * R + 0.357580 * G + 0.180423 * B) / 0.950456
  Y <- 0.212671 * R + 0.715160 * G + 0.072169 * B
  Z <- (0.019334 * R + 0.119193 * G + 0.950227 * B) / 1.088754
  f <- function(t) {
    o <- 7.787 * t + 16 / 116
    i <- t > 0.008856
    o[i] <- t[i]^(1 / 3)
    o
  }
  fX <- f(X)
  fY <- f(Y)
  fZ <- f(Z)
  L <- 903.3 * Y
  i <- Y > 0.008856
  L[i] <- 116 * fY[i] - 16
  list(L = L, a = 500 * (fX - fY), b = 200 * (fY - fZ))
}

# CIEDE2000 colour difference (Sharma et al. 2005) between rows of two Lab matrices.
.ft_de00 <- function(lab1, lab2) {
  lab1 <- matrix(lab1, ncol = 3)
  lab2 <- matrix(lab2, ncol = 3)
  L1 <- lab1[, 1]; a1 <- lab1[, 2]; b1 <- lab1[, 3]
  L2 <- lab2[, 1]; a2 <- lab2[, 2]; b2 <- lab2[, 3]
  rad <- pi / 180
  Cb <- (sqrt(a1^2 + b1^2) + sqrt(a2^2 + b2^2)) / 2
  G <- 0.5 * (1 - sqrt(Cb^7 / (Cb^7 + 25^7)))
  a1p <- (1 + G) * a1
  a2p <- (1 + G) * a2
  C1p <- sqrt(a1p^2 + b1^2)
  C2p <- sqrt(a2p^2 + b2^2)
  h1p <- (atan2(b1, a1p) / rad) %% 360
  h2p <- (atan2(b2, a2p) / rad) %% 360
  zero <- (C1p * C2p) == 0
  dh <- h2p - h1p
  dh <- ifelse(dh > 180, dh - 360, ifelse(dh < -180, dh + 360, dh))
  dh[zero] <- 0
  dLp <- L2 - L1
  dCp <- C2p - C1p
  dHp <- 2 * sqrt(C1p * C2p) * sin(dh / 2 * rad)
  Lbp <- (L1 + L2) / 2
  Cbp <- (C1p + C2p) / 2
  hs <- h1p + h2p
  hbp <- ifelse(abs(h1p - h2p) > 180, ifelse(hs < 360, (hs + 360) / 2, (hs - 360) / 2), hs / 2)
  hbp[zero] <- hs[zero]
  Tt <- 1 - 0.17 * cos((hbp - 30) * rad) + 0.24 * cos(2 * hbp * rad) +
    0.32 * cos((3 * hbp + 6) * rad) - 0.20 * cos((4 * hbp - 63) * rad)
  dtheta <- 30 * exp(-((hbp - 275) / 25)^2)
  RC <- 2 * sqrt(Cbp^7 / (Cbp^7 + 25^7))
  SL <- 1 + 0.015 * (Lbp - 50)^2 / sqrt(20 + (Lbp - 50)^2)
  SC <- 1 + 0.045 * Cbp
  SH <- 1 + 0.015 * Cbp * Tt
  RT <- -sin(2 * dtheta * rad) * RC
  sqrt((dLp / SL)^2 + (dCp / SC)^2 + (dHp / SH)^2 + RT * (dCp / SC) * (dHp / SH))
}

# Erosion with a square (2k + 1) element; pixels outside the image count as
# flower, as in OpenCV's default border for erosion.
.ft_erode <- function(m, k) {
  h <- nrow(m)
  w <- ncol(m)
  p <- cbind(matrix(TRUE, h, k), m, matrix(TRUE, h, k))
  r <- m
  for (d in 0:(2 * k)) r <- r & p[, (1 + d):(w + d), drop = FALSE]
  p <- rbind(matrix(TRUE, k, w), r, matrix(TRUE, k, w))
  o <- r
  for (d in 0:(2 * k)) o <- o & p[(1 + d):(h + d), , drop = FALSE]
  o
}

# Index vector for reflect-101 padding by one pixel.
.ft_r101 <- function(n) if (n > 1L) c(2L, seq_len(n), n - 1L) else c(1L, 1L, 1L)

# OpenCV's Otsu threshold for an 8-bit vector (getThreshVal_Otsu_8u).
.ft_otsu <- function(v) {
  h <- tabulate(v + 1L, nbins = 256L)
  N <- length(v)
  sc <- 1 / N
  mu <- sum((0:255) * h) * sc
  mu1 <- 0
  q1 <- 0
  max_sigma <- 0
  max_val <- 0
  eps <- 1.192092896e-07
  for (i in 0:255) {
    p_i <- h[i + 1L] * sc
    mu1 <- mu1 * q1
    q1 <- q1 + p_i
    q2 <- 1 - q1
    if (min(q1, q2) < eps || max(q1, q2) > 1 - eps) next
    mu1 <- (mu1 + i * p_i) / q1
    mu2 <- (mu - q1 * mu1) / q2
    sigma <- q1 * q2 * (mu1 - mu2)^2
    if (sigma > max_sigma) {
      max_sigma <- sigma
      max_val <- i
    }
  }
  max_val
}

# Convex-hull area of the first and last pixel centre of every row (as the back end).
.ft_hull_solidity <- function(mask) {
  h <- nrow(mask)
  idx <- which(mask)
  if (!length(idx)) return(NA_real_)
  ys <- (idx - 1L) %% h + 1L
  xs <- (idx - 1L) %/% h + 1L
  first <- !duplicated(ys)
  last <- !duplicated(ys, fromLast = TRUE)
  hx <- c(xs[first], xs[last])
  hy <- c(ys[first], ys[last])
  ch <- grDevices::chull(hx, hy)
  hull <- abs(sum(hx[ch] * c(hy[ch][-1], hy[ch][1]) - c(hx[ch][-1], hx[ch][1]) * hy[ch])) / 2
  if (hull > 0) length(idx) / hull else NA_real_
}

.ft_quality <- function(img, mask, min_px) {
  h <- nrow(mask)
  w <- ncol(mask)
  px <- sum(mask)
  na <- NA_real_
  q <- list(px = as.integer(px), mask_frac = px / (h * w), border_touch = na,
            solidity = na, clip_hi = na, clip_lo = na, sharp = na, mask_ok = FALSE)
  if (px == 0) return(q)
  q$border_touch <- mean(c(mask[1, ], mask[h, ], mask[, 1], mask[, w]))
  idx <- which(mask)
  q$solidity <- .ft_hull_solidity(mask)
  mx <- pmax(img[, , 1][idx], img[, , 2][idx], img[, , 3][idx])
  q$clip_hi <- mean(mx >= 250)
  q$clip_lo <- mean(mx <= 5)
  gray <- (img[, , 1] * 4899 + img[, , 2] * 9617 + img[, , 3] * 1868 + 8192) %/% 16384
  g <- gray[.ft_r101(h), .ft_r101(w), drop = FALSE]
  lap <- g[1:h, 2:(w + 1), drop = FALSE] + g[3:(h + 2), 2:(w + 1), drop = FALSE] +
    g[2:(h + 1), 1:w, drop = FALSE] + g[2:(h + 1), 3:(w + 2), drop = FALSE] - 4 * gray
  inner <- .ft_erode(mask, 2L)
  if (sum(inner) > 50) {
    v <- lap[inner]
    q$sharp <- mean((v - mean(v))^2)
  }
  q$mask_ok <- px >= min_px && q$border_touch <= 0.10
  q
}

.ft_core_names <- c("D_px", "L_mean", "L_sd", "C_mean", "a_mean", "b_mean", "hue_deg", "hue_conc", "vap_wp",
                    "dark_frac_otsu", "L_entropy")

.ft_core <- function(img, mask, min_px) {
  out <- stats::setNames(as.list(rep(NA_real_, length(.ft_core_names))), .ft_core_names)
  geo <- .ft_geometry_empty()
  rad <- .ft_radial_empty()
  px <- sum(mask)
  if (px < min_px) return(c(out, geo, rad, list(colour_name = NA_character_)))
  R <- .ft_lin(img[, , 1])
  G <- .ft_lin(img[, , 2])
  B <- .ft_lin(img[, , 3])
  Yall <- 0.2126 * R + 0.7152 * G + 0.0722 * B
  y99 <- stats::quantile(Yall, 0.99, names = FALSE, type = 7) + 1e-6
  idx <- which(mask)
  lab <- .ft_lab(R[idx], G[idx], B[idx])
  L <- .ft_f32(lab$L)
  a <- lab$a
  b <- lab$b
  C <- sqrt(a^2 + b^2)
  hx <- sum(a) / (sum(C) + 1e-6)
  hy <- sum(b) / (sum(C) + 1e-6)
  D <- 2 * sqrt(px / pi)
  Y <- Yall[idx]
  L8 <- floor(pmin(pmax(.ft_f32(L * .ft_f32(2.55)), 0), 255))
  thr <- .ft_otsu(as.integer(L8))
  Lin <- L[L >= 0 & L <= 100]
  bins <- pmin(floor(Lin / 2), 49)
  p <- tabulate(bins + 1L, nbins = 50L)
  p <- p[p > 0] / sum(p)
  out$D_px <- round(D, 1)
  out$L_mean <- mean(L)
  out$L_sd <- sqrt(mean((L - mean(L))^2))
  out$C_mean <- mean(C)
  out$a_mean <- mean(a)
  out$b_mean <- mean(b)
  out$hue_deg <- (atan2(hy, hx) * 180 / pi) %% 360
  out$hue_conc <- sqrt(hx^2 + hy^2)
  out$vap_wp <- 1 - min(1, mean(Y) / y99)
  out$dark_frac_otsu <- mean(L8 <= thr)
  out$L_entropy <- -sum(p * log2(p))
  g <- .ft_geometry(mask)
  rad <- .ft_radial(mask, g, cbind(L, a, b))
  nm <- if (is.finite(rad$petal_L)) ft_colour_name(rad$petal_L, rad$petal_a, rad$petal_b) else NA_character_
  c(out, g[names(geo)], rad, list(colour_name = nm))
}
