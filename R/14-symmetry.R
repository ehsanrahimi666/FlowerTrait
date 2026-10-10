#' Symmetry, petal number and outline shape of a flower
#'
#' Measures the geometry of a flower outline, natively in R and identically to
#' the 'Python' back end. The outline is first reduced (area averaging) so that
#' its equivalent diameter is at most 128 px, which makes the traits comparable
#' between photographs taken at different distances. The radius of the outline
#' is then measured along 360 rays from its centroid (the farthest outline pixel
#' on each ray), and:
#' \itemize{
#'   \item `petal_n` is the dominant harmonic (3 to 24) of the radius function,
#'     that is, the number of petals, lobes or ray florets that best explains
#'     the outline; `petal_power` is the share of the radius variation at that
#'     harmonic and `lobe_depth` its amplitude relative to the mean radius
#'     (about 0 for an entire, disc-like outline);
#'   \item `sym_rotational` is the overlap (intersection over union) between the
#'     outline and a copy rotated by one petal (360 / `petal_n` degrees);
#'     radially symmetric flowers seen face-on score close to 1;
#'   \item `sym_bilateral` is the overlap between the outline and its mirror
#'     image about the best axis through the centroid (`sym_axis_deg`; axes are
#'     tested every 5 degrees and refined in 0.5-degree steps);
#'   \item `radius_cv` (coefficient of variation of the radius), `fill_circle`
#'     (outline area relative to the circle through its farthest point),
#'     `aspect` (square root of the ratio of the principal second moments) and
#'     `solidity_shape` (area relative to the convex hull).
#' }
#' Petal number and rotational symmetry describe the flower only when it is
#' seen face-on; use them with photographs kept as face views by [ft_filter()].
#'
#' @param mask Logical outline matrix or path to a PNG mask; several paths
#'   process several outlines.
#' @param profile If `TRUE`, the radius function, the centroid and the working
#'   outline are returned in `attr(result, "profile")` (one outline only).
#' @return A data frame with one row per outline and the columns described
#'   above (and `photo_id` for file input).
#' @seealso [ft_traits()], [ft_plot_symmetry()]
#' @examples
#' # synthetic outlines: five petals, six petals and a bilateral (zygomorphic) shape
#' yy <- row(matrix(0, 240, 240)) - 120; xx <- col(matrix(0, 240, 240)) - 120
#' th <- atan2(yy, xx); rr <- sqrt(xx^2 + yy^2)
#' five <- rr < 70 + 30 * cos(5 * th)
#' six <- rr < 70 + 25 * cos(6 * th)
#' zygo <- rr < (60 + 20 * cos(5 * th)) * (1 + 0.5 * cos(th))
#' ft_symmetry(five)[, c("petal_n", "lobe_depth", "sym_rotational", "sym_bilateral")]
#' rbind(five = ft_symmetry(five), six = ft_symmetry(six), zygomorphic = ft_symmetry(zygo))
#'
#' ex <- ft_example_photos()
#' ft_symmetry(ex$mask[1:3])
#' @export
ft_symmetry <- function(mask, profile = FALSE) {
  if (is.character(mask) && length(mask) > 1L) {
    out <- do.call(rbind, lapply(mask, ft_symmetry))
    rownames(out) <- NULL
    return(out)
  }
  id <- if (is.character(mask)) tools::file_path_sans_ext(basename(mask)) else NULL
  m <- .ft_as_mask(mask)
  g <- .ft_geometry(m)
  out <- as.data.frame(g[names(.ft_geometry_empty())])
  if (!is.null(id)) out <- cbind(photo_id = id, out, stringsAsFactors = FALSE)
  if (profile) attr(out, "profile") <- attr(g, "profile")
  out
}

#' Plot the outline radius and the petal harmonic of a flower
#'
#' Draws the crop with the outline and the centre and periphery zones used for
#' the radial colour traits (left), and the radius of the outline against the
#' angle, with the harmonic chosen as the petal number (right).
#'
#' @param img Crop (RGB array or file path) or `NULL` to plot the profile only.
#' @param mask Outline matrix or path.
#' @param col Colour of the outline and profile.
#' @return The output of [ft_symmetry()], invisibly.
#' @examples
#' ex <- ft_example_photos()
#' ft_plot_symmetry(ex$crop[2], ex$mask[2])
#' @export
ft_plot_symmetry <- function(img, mask, col = "#E8A317") {
  s <- ft_symmetry(mask, profile = TRUE)
  pr <- attr(s, "profile")
  if (is.null(pr)) stop("The outline is too small for a radius profile.", call. = FALSE)
  m <- .ft_as_mask(mask)
  if (!is.null(img)) {
    op <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(op))
    im <- .ft_as_image(img)
    .ft_panel(im, m, col, 2, NULL)
    sc <- 1 / pr$scale
    ang <- seq(0, 2 * pi, length.out = 361)[-361]
    for (f in c(0.3, 0.6)) {
      x <- (pr$cx + f * pr$r * cos(ang)) * sc + 1
      y <- (pr$cy + f * pr$r * sin(ang)) * sc + 1
      graphics::polygon(x - 0.5, nrow(m) - y + 0.5, border = "white", lty = 2, lwd = 1.2)
    }
  }
  r <- pr$r
  k <- s$petal_n
  th <- seq(0, 360, length.out = length(r) + 1)[-(length(r) + 1)]
  F <- stats::fft(r) / length(r)
  fit <- Re(F[1]) + 2 * Re(F[k + 1] * exp(1i * k * th * pi / 180))
  op2 <- graphics::par(mar = c(4, 4, 2, 1))
  on.exit(graphics::par(op2), add = TRUE)
  graphics::plot(th, r / mean(r), type = "l", lwd = 1.6, col = col, xlab = "Angle (degrees)",
                 ylab = "Radius / mean radius", xaxt = "n",
                 main = sprintf("petal_n = %d, lobe depth = %.2f", k, s$lobe_depth), cex.main = 0.9)
  graphics::axis(1, at = seq(0, 360, 90))
  graphics::lines(th, fit / mean(r), col = "grey30", lty = 2)
  invisible(s)
}

.ft_geom_keys <- c("sym_bilateral", "sym_axis_deg", "sym_rotational", "petal_n", "petal_power", "lobe_depth",
                   "radius_cv", "fill_circle", "aspect", "solidity_shape")
.ft_radial_keys <- c("centre_dL", "centre_dE", "centre_frac", "petal_L", "petal_a", "petal_b")

.ft_geometry_empty <- function() stats::setNames(as.list(rep(NA_real_, length(.ft_geom_keys))), .ft_geom_keys)
.ft_radial_empty <- function() stats::setNames(as.list(rep(NA_real_, length(.ft_radial_keys))), .ft_radial_keys)

# Working outline: area-averaged so that D <= 128 px, thresholded at 0.5 (as the back end).
.ft_work_mask <- function(m, work_d = 128) {
  D <- 2 * sqrt(sum(m) / pi)
  if (D <= work_d) return(list(mask = m, scale = 1))
  s <- work_d / D
  h <- max(1, floor(nrow(m) * s + 0.5))
  w <- max(1, floor(ncol(m) * s + 0.5))
  list(mask = .ft_area_resize(m * 1, h, w) > 0.5, scale = s)
}

# Pixel lookup of transformed (0-based) pixel centres; returns the IoU with the outline.
.ft_overlap <- function(m, ty, tx, n) {
  h <- nrow(m)
  w <- ncol(m)
  ry <- floor(ty + 0.5)
  rx <- floor(tx + 0.5)
  ok <- ry >= 0 & ry < h & rx >= 0 & rx < w
  hit <- logical(n)
  hit[ok] <- m[cbind(ry[ok] + 1, rx[ok] + 1)]
  k <- sum(hit)
  k / (2 * n - k)
}

.ft_radius <- function(m, cy, cx, n_ray = 360, step = 0.25) {
  h <- nrow(m)
  w <- ncol(m)
  th <- 2 * pi * (0:(n_ray - 1)) / n_ray
  nd <- ceiling((sqrt(h^2 + w^2) + step) / step)
  d <- (0:(nd - 1)) * step
  yy <- floor(cy + outer(sin(th), d) + 0.5)
  xx <- floor(cx + outer(cos(th), d) + 0.5)
  ok <- yy >= 0 & yy < h & xx >= 0 & xx < w
  hit <- matrix(FALSE, n_ray, nd)
  hit[ok] <- m[cbind(yy[ok] + 1, xx[ok] + 1)]
  last <- apply(hit, 1, function(z) if (any(z)) max(which(z)) else NA_integer_)
  ifelse(is.na(last), 0, d[last])
}

.ft_geometry <- function(mask) {
  out <- .ft_geometry_empty()
  wk <- .ft_work_mask(mask)
  wm <- wk$mask
  if (sum(wm) < 100) return(out)
  h <- nrow(wm)
  idx <- which(t(wm))  # row-major order, as numpy
  ys <- (idx - 1L) %/% ncol(wm)
  xs <- (idx - 1L) %% ncol(wm)
  n <- length(ys)
  cy <- mean(ys)
  cx <- mean(xs)
  r <- .ft_radius(wm, cy, cx)
  r0 <- mean(r)
  if (r0 <= 0) return(out)
  F <- Mod(stats::fft(r)) / length(r)
  ks <- 3:24
  kk <- ks[which.max(F[ks + 1])]
  dy <- ys - cy
  dx <- xs - cx
  a <- 2 * pi / kk
  rot <- .ft_overlap(wm, cy + dx * sin(a) + dy * cos(a), cx + dx * cos(a) - dy * sin(a), n)
  mirror <- function(deg) {
    phi <- deg * pi / 180
    ux <- cos(phi)
    uy <- sin(phi)
    t <- dx * ux + dy * uy
    .ft_overlap(wm, cy + 2 * t * uy - dy, cx + 2 * t * ux - dx, n)
  }
  best <- -1
  best_ax <- NA_real_
  for (deg in 180 * (0:35) / 36) {
    v <- mirror(deg)
    if (v > best) {
      best <- v
      best_ax <- deg
    }
  }
  cand <- best_ax + 0.5 * setdiff(-10:10, 0)
  for (deg in cand) {
    v <- mirror(deg)
    if (v > best) {
      best <- v
      best_ax <- deg %% 180
    }
  }
  vx <- mean((xs - cx)^2)
  vy <- mean((ys - cy)^2)
  cxy <- mean((xs - cx) * (ys - cy))
  ev <- sort(eigen(matrix(c(vx, cxy, cxy, vy), 2), symmetric = TRUE, only.values = TRUE)$values,
             decreasing = TRUE)
  out$sym_bilateral <- best
  out$sym_axis_deg <- best_ax
  out$sym_rotational <- rot
  out$petal_n <- kk
  out$petal_power <- F[kk + 1] / sum(F[2:(length(r) / 2 + 1)])
  out$lobe_depth <- 2 * F[kk + 1] / r0
  out$radius_cv <- sqrt(mean((r - r0)^2)) / r0
  out$fill_circle <- n / (pi * max(r)^2)
  out$aspect <- sqrt(ev[1] / max(ev[2], 1e-12))
  out$solidity_shape <- .ft_hull_solidity(wm)
  attr(out, "profile") <- list(r = r, cy = cy, cx = cx, scale = wk$scale, mask = wm)
  out
}

# Centre versus petal periphery colour (relative radius < 0.3 and > 0.6).
.ft_radial <- function(mask, g, lab) {
  out <- .ft_radial_empty()
  pr <- attr(g, "profile")
  if (is.null(pr)) return(out)
  h <- nrow(mask)
  idx <- which(mask)
  ys <- (idx - 1L) %% h
  xs <- (idx - 1L) %/% h
  cy <- mean(ys)
  cx <- mean(xs)
  ang <- atan2(ys - cy, xs - cx) %% (2 * pi)
  j <- (floor(ang / (2 * pi) * 360 + 0.5)) %% 360
  rr <- pr$r[j + 1] / pr$scale
  rel <- sqrt((ys - cy)^2 + (xs - cx)^2) / pmax(rr, 1e-9)
  cen <- rel < 0.3
  per <- rel > 0.6
  if (sum(cen) < 20 || sum(per) < 20) return(out)
  cc <- colMeans(lab[cen, , drop = FALSE])
  pp <- colMeans(lab[per, , drop = FALSE])
  out$centre_dL <- unname(cc[1] - pp[1])
  out$centre_dE <- .ft_de00(cc, pp)
  out$centre_frac <- mean(cen)
  out$petal_L <- unname(pp[1])
  out$petal_a <- unname(pp[2])
  out$petal_b <- unname(pp[3])
  out
}
