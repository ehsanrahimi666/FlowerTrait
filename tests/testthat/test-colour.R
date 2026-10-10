img_flower <- function() {
  img <- array(0, c(120, 120, 3))
  img[, , 1] <- 70; img[, , 2] <- 120; img[, , 3] <- 60
  yy <- row(img[, , 1]) - 60; xx <- col(img[, , 1]) - 60
  m <- sqrt(xx^2 + yy^2) < 35 + 12 * cos(5 * atan2(yy, xx))
  img[, , 1][m] <- 230; img[, , 2][m] <- 90; img[, , 3][m] <- 160
  grey <- xx > 45
  img[, , 1][grey] <- 150; img[, , 2][grey] <- 140; img[, , 3][grey] <- 125
  list(img = img, mask = m)
}

test_that("white balance keeps brightness and neutralises a cast", {
  x <- img_flower()
  for (m in c("gray_world", "white_patch", "shades_of_gray", "gray_pixels")) {
    w <- ft_white_balance(x$img, m)
    expect_equal(dim(w), dim(x$img))
    expect_true(all(w >= 0 & w <= 255))
  }
  w <- ft_white_balance(x$img, "gray_pixels", mask = x$mask, use = "background")
  bg <- !x$mask & (col(x$mask) > 105)
  expect_lt(abs(mean(w[, , 1][bg]) - mean(w[, , 3][bg])), abs(mean(x$img[, , 1][bg]) - mean(x$img[, , 3][bg])))
})

test_that("colour check reports casts, clipping and exposure", {
  x <- img_flower()
  cc <- ft_colour_check(x$img, x$mask)
  expect_named(cc, c("neutral", "cast_rg", "cast_bg", "cast", "gw_rg", "gw_bg", "clip", "dark", "issues"))
  expect_gt(cc$cast, 0.1)
  dark <- x$img * 0.01
  expect_match(ft_colour_check(dark, x$mask)$issues, "under-exposed")
  ex <- ft_example_photos()
  cc2 <- ft_colour_check(ex$crop[1:2], ex$mask[1:2])
  expect_equal(nrow(cc2), 2)
  expect_true("photo_id" %in% names(cc2))
})

test_that("augmentations change colour but quarter turns do not", {
  x <- img_flower()
  base <- ft_traits(x$img, x$mask)
  r90 <- ft_augment(x$img, x$mask, "rotate90", value = 1)
  t90 <- ft_traits(r90$image, r90$mask)
  expect_equal(t90$L_mean, base$L_mean, tolerance = 1e-10)
  expect_equal(t90$petal_n, base$petal_n)
  fl <- ft_augment(x$img, x$mask, "flip", value = "horizontal")
  expect_equal(ft_traits(fl$image, fl$mask)$C_mean, base$C_mean, tolerance = 1e-10)
  for (tp in c("white_balance", "exposure", "jpeg", "blur", "brightness", "contrast", "saturation", "hue",
               "rotate", "perspective")) {
    a <- ft_augment(x$img, x$mask, tp)
    expect_equal(dim(a$image), dim(x$img))
    expect_true(all(a$image >= 0 & a$image <= 255))
  }
  h <- ft_augment(x$img, x$mask, "hue", value = 0.1)
  expect_false(isTRUE(all.equal(ft_traits(h$image, h$mask)$hue_deg, base$hue_deg)))
  chk <- ft_augment_check(x$img, x$mask, types = c("rotate90", "saturation"))
  # quarter turns change colour traits not at all and the radius function only by ray rounding
  expect_true(all(chk$rel_change[chk$type == "rotate90"] < 1e-3, na.rm = TRUE))
  col_tr <- c("L_mean", "C_mean", "a_mean", "b_mean", "petal_L", "centre_dE")
  expect_true(all(chk$rel_change[chk$type == "rotate90" & chk$trait %in% col_tr] < 1e-8, na.rm = TRUE))
  expect_true(any(chk$flag[chk$type == "saturation"]))
})
