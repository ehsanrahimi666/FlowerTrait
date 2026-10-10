#' Human colour category of CIELAB colours
#'
#' Assigns one of 11 basic colour categories (white, grey, black, red, pink,
#' orange, yellow, brown, green, blue, purple) to CIELAB colours (D65), with a
#' fixed rule on hue angle, chroma and lightness. The rule is the same as in
#' the 'Python' back end and is applied to the petal periphery of each flower
#' (`colour_name` in [ft_traits()]) and to its dominant colour class
#' (`dominant_name`, back end).
#'
#' Colours with chroma below `achromatic_C` are white (`L*` >= 60), grey (30-60)
#' or black (< 30). Chromatic colours are named by the CIELAB hue angle:
#' 345-20 degrees pink (`L*` >= 45) or red; 20-50 red (pink if `L*` >= 65 and
#' chroma < 50; brown if `L*` < 35 and chroma < 40); 50-80 orange (brown if
#' `L*` < 45); 80-115 yellow (brown if `L*` < 45); 115-200 green; 200-310 blue;
#' 310-345 purple. The sector limits follow the hue angles of the sRGB primary
#' and secondary colours (red 40, orange 73, yellow 103, green 136, cyan 196,
#' blue 306 and magenta 328 degrees). Names describe colour as it appears in an
#' sRGB photograph, not as pollinators see it.
#'
#' @param L,a,b CIELAB coordinates (vectors of equal length), or a three-column
#'   matrix in `L`.
#' @param achromatic_C Chroma below which a colour counts as achromatic.
#' @return A character vector of colour names.
#' @seealso [ft_traits()], [ft_spectra_to_lab()], [ft_plot_colours()]
#' @examples
#' ft_colour_name(c(47, 80, 90, 50), c(62, 5, -1, 20), c(50, 75, 8, -35))
#' lab <- ft_rgb_to_lab(rbind(poppy = c(220, 40, 30), harebell = c(120, 120, 220),
#'                            daisy = c(245, 245, 240), buttercup = c(250, 210, 30)))
#' ft_colour_name(lab)
#' @export
ft_colour_name <- function(L, a = NULL, b = NULL, achromatic_C = 15) {
  if (is.matrix(L) && ncol(L) == 3L) {
    a <- L[, 2]
    b <- L[, 3]
    L <- L[, 1]
  }
  if (is.null(a) || is.null(b) || length(a) != length(L) || length(b) != length(L)) {
    stop("Give `L`, `a` and `b` of equal length, or a three-column matrix.", call. = FALSE)
  }
  C <- sqrt(a^2 + b^2)
  h <- (atan2(b, a) * 180 / pi) %% 360
  out <- rep(NA_character_, length(L))
  ach <- !is.na(C) & C < achromatic_C
  out[ach] <- ifelse(L[ach] >= 60, "white", ifelse(L[ach] >= 30, "grey", "black"))
  ch <- !is.na(C) & !ach
  s1 <- ch & (h >= 345 | h < 20)
  out[s1] <- ifelse(L[s1] >= 45, "pink", "red")
  s2 <- ch & h >= 20 & h < 50
  out[s2] <- ifelse(L[s2] >= 65 & C[s2] < 50, "pink", ifelse(L[s2] < 35 & C[s2] < 40, "brown", "red"))
  s3 <- ch & h >= 50 & h < 80
  out[s3] <- ifelse(L[s3] < 45, "brown", "orange")
  s4 <- ch & h >= 80 & h < 115
  out[s4] <- ifelse(L[s4] < 45, "brown", "yellow")
  out[ch & h >= 115 & h < 200] <- "green"
  out[ch & h >= 200 & h < 310] <- "blue"
  out[ch & h >= 310 & h < 345] <- "purple"
  out
}

#' Convert reflectance spectra to CIELAB
#'
#' Computes the CIELAB colour of reflectance spectra under illuminant D65 for
#' the CIE 1931 2-degree standard observer, the colour space of the traits of
#' [ft_traits()]. Spectra are interpolated to 1 nm, held constant beyond the
#' measured range (most floral spectra end at 700 nm) and integrated from 360
#' to 830 nm; the white point is the perfect reflecting diffuser. Use it to
#' compare traits measured in photographs with spectrophotometric
#' measurements, as in [ft_reference].
#'
#' @param wavelength Wavelengths (nm), increasing.
#' @param reflectance Reflectance: a vector, or a matrix with one column per
#'   spectrum. Values are clipped to 0-1.2 after scaling (fluorescent or glossy
#'   samples can exceed 1).
#' @param percent Are reflectance values percentages? `NULL` (default) decides
#'   for all spectra together: percentages when any value exceeds 1.5.
#' @return A matrix with columns `L`, `a`, `b`, `C` (chroma) and `h` (hue angle,
#'   degrees), one row per spectrum.
#' @seealso [ft_colour_name()], [ft_reference]
#' @examples
#' wl <- 300:700
#' grey50 <- rep(50, length(wl))                 # 50% reflectance, in percent
#' yellow <- 5 + 85 / (1 + exp(-(wl - 510) / 8)) # long-pass, like many yellow petals
#' lab <- ft_spectra_to_lab(wl, cbind(grey50, yellow))
#' lab
#' ft_colour_name(lab[, 1:3])
#' @export
ft_spectra_to_lab <- function(wavelength, reflectance, percent = NULL) {
  R <- as.matrix(reflectance)
  if (nrow(R) != length(wavelength)) R <- t(R)
  if (nrow(R) != length(wavelength)) stop("`reflectance` does not match `wavelength`.", call. = FALSE)
  if (is.unsorted(wavelength)) stop("`wavelength` must be increasing.", call. = FALSE)
  if (is.null(percent)) percent <- any(R > 1.5, na.rm = TRUE)
  if (percent) R <- R / 100
  cie <- .ft_cie
  E <- cie$D65
  k <- 100 / sum(E * cie$ybar)
  white <- k * c(sum(E * cie$xbar), sum(E * cie$ybar), sum(E * cie$zbar))
  out <- t(apply(R, 2, function(r) {
    ok <- is.finite(r)
    if (sum(ok) < 10) return(rep(NA_real_, 5))
    r <- r[ok]
    w <- wavelength[ok]
    r <- pmin(pmax(r, 0), 1.2)
    full <- stats::approx(w, r, xout = cie$wavelength, rule = 2)$y
    XYZ <- k * c(sum(E * full * cie$xbar), sum(E * full * cie$ybar), sum(E * full * cie$zbar))
    f <- function(t) ifelse(t > (6 / 29)^3, t^(1 / 3), t / (3 * (6 / 29)^2) + 4 / 29)
    fx <- f(XYZ[1] / white[1])
    fy <- f(XYZ[2] / white[2])
    fz <- f(XYZ[3] / white[3])
    L <- 116 * fy - 16
    a <- 500 * (fx - fy)
    b <- 200 * (fy - fz)
    c(L, a, b, sqrt(a^2 + b^2), (atan2(b, a) * 180 / pi) %% 360)
  }))
  colnames(out) <- c("L", "a", "b", "C", "h")
  rownames(out) <- colnames(R)
  out
}
