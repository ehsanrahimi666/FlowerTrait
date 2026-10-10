test_that("R traits match the Python back end on the example crops", {
  ref <- read.csv(test_path("fixtures", "python_core_traits.csv"),
                  colClasses = c(photo_id = "character"))
  ex <- ft_example_photos()
  i <- match(ref$photo_id, ex$photo_id)
  tr <- ft_traits(ex$crop[i], ex$mask[i])
  expect_equal(tr$photo_id, ref$photo_id)
  ok <- !is.na(ref$L_mean)
  # Outline traits depend only on the masks (lossless PNG) and must match on every platform.
  for (k in c("px", "border_touch", "D_px", "petal_n", "sym_rotational", "sym_bilateral", "fill_circle",
              "solidity_shape")) {
    expect_equal(tr[[k]][ok], ref[[k]][ok], tolerance = 1e-10, info = k)
  }
  for (k in c("solidity", "lobe_depth", "radius_cv", "aspect")) {
    expect_equal(tr[[k]][ok], ref[[k]][ok], tolerance = 1e-4, info = k)
  }
  # Pixel traits. The fixture was computed from crops decoded with libjpeg-turbo; other JPEG decoders
  # (such as the IJG libjpeg used by some Windows and macOS builds of 'jpeg') return slightly different
  # pixels. Traits are therefore compared exactly only when the crops decode to the reference pixels,
  # and otherwise within the size of the decoder differences.
  px <- t(vapply(ex$crop[i], function(f) {
    x <- ft_read_image(f)
    c(sum(x), sum(x^2))
  }, numeric(2)))
  same <- isTRUE(all.equal(unname(px), unname(cbind(ref$pixel_sum, ref$pixel_ss)), tolerance = 1e-12))
  for (k in c("clip_hi", "clip_lo", "dark_frac_otsu")) {
    expect_equal(tr[[k]][ok], ref[[k]][ok], tolerance = if (same) 1e-10 else 0.02, info = k)
  }
  for (k in c("sharp", "L_mean", "L_sd", "C_mean", "a_mean", "b_mean", "hue_conc", "vap_wp", "L_entropy",
              "centre_dL", "centre_dE", "petal_L", "petal_a", "petal_b")) {
    expect_equal(tr[[k]][ok], ref[[k]][ok], tolerance = if (same) 1e-4 else 0.02, info = k)
  }
  dh <- abs(((tr$hue_deg - ref$hue_deg + 180) %% 360) - 180)[ok]
  expect_lt(max(dh), if (same) 1e-3 else 1)
})

test_that("traits behave as expected on a synthetic two-tone disk", {
  img <- array(150, c(120, 120, 3))
  yy <- row(img[, , 1])
  xx <- col(img[, , 1])
  disk <- (yy - 60)^2 + (xx - 60)^2 < 45^2
  img[, , 1][disk] <- 220
  img[, , 2][disk] <- 120
  img[, , 3][disk] <- 30
  left <- disk & xx < 60
  img[, , 1][left] <- 30
  img[, , 2][left] <- 25
  img[, , 3][left] <- 20
  t <- ft_traits(img, disk)
  expect_equal(t$px, sum(disk))
  expect_true(t$mask_ok)
  expect_equal(t$border_touch, 0)
  expect_gt(t$solidity, 0.95)
  expect_equal(t$dark_frac_otsu, mean(xx[disk] < 60), tolerance = 1e-12)
  expect_gt(t$C_mean, 10)
  expect_equal(t$D_px, round(2 * sqrt(sum(disk) / pi), 1))
  expect_gt(t$sym_rotational, 0.95)
  expect_gt(t$fill_circle, 0.9)
  small <- ft_traits(img, disk & yy < 52 & yy > 48)
  expect_true(is.na(small$L_mean))
  expect_false(small$mask_ok)
})

test_that("a dark centre gives a negative centre-periphery contrast", {
  img <- array(0, c(160, 160, 3))
  yy <- row(img[, , 1]) - 80
  xx <- col(img[, , 1]) - 80
  rr <- sqrt(yy^2 + xx^2)
  disk <- rr < 70
  img[, , 1][disk] <- 245
  img[, , 2][disk] <- 215
  img[, , 3][disk] <- 40
  centre <- rr < 15
  img[, , 1][centre] <- 40
  img[, , 2][centre] <- 25
  img[, , 3][centre] <- 15
  t <- ft_traits(img, disk)
  expect_lt(t$centre_dL, -30)
  expect_gt(t$centre_dE, 20)
  expect_equal(t$colour_name, "yellow")
})

test_that("CIELAB conversion gives reference values", {
  lab <- ft_rgb_to_lab(rbind(c(255, 255, 255), c(0, 0, 0), c(255, 0, 0)))
  expect_equal(unname(lab[1, ]), c(100, 0, 0), tolerance = 1e-3)
  expect_equal(unname(lab[2, ]), c(0, 0, 0), tolerance = 1e-6)
  expect_equal(unname(lab[3, 1]), 53.24, tolerance = 1e-3)
  ex <- ft_example_photos()
  a <- ft_rgb_to_lab(ft_read_image(ex$crop[1]))
  expect_equal(dim(a)[3], 3L)
})

test_that("input errors are informative", {
  expect_error(ft_traits(array(100, c(5, 5, 3)), matrix(TRUE, 4, 4)), "same height")
  expect_error(ft_traits(matrix(1, 5, 5), matrix(TRUE, 5, 5)), "array")
  expect_error(ft_traits(c("a.jpg", "b.jpg"), "a.png"), "one mask")
  expect_warning(ft_traits(array(0.5, c(30, 30, 3)), matrix(TRUE, 30, 30)), "0-1")
})
