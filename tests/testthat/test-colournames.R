test_that("colour names follow the documented rule", {
  expect_equal(ft_colour_name(c(90, 47, 80, 50, 60, 70, 60, 50, 35, 40, 20),
                              c(-1, 62, 5, 20, 40, 30, -25, 50, 50, 0, 2),
                              c(8, 50, 75, -35, -10, 75, 30, -25, 25, 2, 1)),
               c("white", "red", "yellow", "blue", "pink", "orange", "green", "purple", "red", "grey", "black"))
  # light, unsaturated red is pink; dark, dull red-orange is brown
  expect_equal(ft_colour_name(80, 20, 10), "pink")
  expect_equal(ft_colour_name(30, 15, 20), "brown")
  expect_equal(ft_colour_name(40, 5, 40), "brown")
  # matrix input and missing values
  m <- rbind(c(90, 0, 5), c(NA, 1, 1))
  expect_equal(ft_colour_name(m), c("white", NA))
  expect_error(ft_colour_name(1:2, 1), "equal length")
})

test_that("sRGB anchor colours fall in their own sectors", {
  lab <- ft_rgb_to_lab(rbind(red = c(255, 0, 0), orange = c(255, 140, 0), yellow = c(255, 230, 0),
                             green = c(0, 160, 0), blue = c(40, 60, 230), violet = c(160, 60, 200),
                             white = c(250, 250, 250), pink = c(255, 170, 190)))
  expect_equal(unname(ft_colour_name(lab)),
               c("red", "orange", "yellow", "green", "blue", "purple", "white", "pink"))
})

test_that("reflectance spectra are converted to CIELAB", {
  wl <- 300:700
  lab <- ft_spectra_to_lab(wl, cbind(white = rep(100, length(wl)), grey = rep(18.4, length(wl))))
  expect_equal(unname(lab["white", c("L", "a", "b")]), c(100, 0, 0), tolerance = 1e-8)
  expect_equal(unname(lab["grey", "L"]), 50, tolerance = 0.05)
  expect_lt(abs(lab["grey", "a"]), 1e-8)
  # fractions and percentages give the same colour
  yellow <- 0.05 + 0.85 / (1 + exp(-(wl - 510) / 8))
  expect_equal(ft_spectra_to_lab(wl, yellow), ft_spectra_to_lab(wl, 100 * yellow), ignore_attr = TRUE)
  y <- ft_spectra_to_lab(wl, yellow)
  expect_equal(ft_colour_name(y[, 1:3, drop = FALSE]), "yellow")
  expect_gt(y[, "C"], 60)
  expect_error(ft_spectra_to_lab(400:410, 1:3), "does not match")
})

test_that("the percentage scale is decided for all spectra together", {
  wl <- 300:700
  dark <- rep(1, length(wl))          # 1% reflectance
  bright <- rep(60, length(wl))       # 60% reflectance
  lab <- ft_spectra_to_lab(wl, cbind(dark, bright))
  expect_equal(unname(lab[1, "L"]), 903.3 * 0.01, tolerance = 0.02)
  expect_equal(unname(ft_spectra_to_lab(wl, dark, percent = TRUE)[1, "L"]), unname(lab[1, "L"]))
  expect_equal(unname(ft_spectra_to_lab(wl, dark / 100)[1, "L"]), unname(lab[1, "L"]))
})
