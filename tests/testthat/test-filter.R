test_that("reasons follow the order of checks", {
  base <- data.frame(mask_ok = TRUE, stab = 0.95, p_usable = 0.9, p_face_view = 0.8)
  x <- base[rep(1, 5), ]
  x$mask_ok[1] <- FALSE
  x$stab[2] <- 0.5
  x$p_usable[3] <- 0.2
  x$p_face_view[4] <- 0.1
  x$error <- c(NA, NA, NA, NA, "download failed")
  d <- ft_filter(x, "default")
  expect_equal(d$reason, c("outline failed QC", "face view",
                           "unclear photo (no open flower, hidden, blurry or cut off)",
                           "side view", "processing error"))
  expect_equal(d$keep, c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(d$view, c(NA, "face", NA, "side", NA))
  s <- ft_filter(x, "strict")
  expect_equal(s$reason[2], "unstable outline")
  expect_equal(s$keep, c(FALSE, FALSE, FALSE, TRUE, FALSE))
  fv <- ft_filter(x, "face_view")
  expect_equal(fv$keep, rep(FALSE, 5))
  expect_equal(fv$reason[4], "side view")
  expect_true(all(ft_filter(x[1:4, ], "none")$keep))
  expect_equal(ft_filter(x, "qc")$keep, c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_error(ft_filter(x[, -1], "default"), "mask_ok")
  expect_error(ft_filter(x[, c("mask_ok", "stab")], "default"), "p_usable")
  expect_error(ft_filter(1:3), "data frame")
})

test_that("thresholds can be changed", {
  x <- data.frame(mask_ok = TRUE, stab = 0.85, p_usable = c(0.4, 0.6), p_face_view = 0.6)
  expect_equal(ft_filter(x)$keep, c(FALSE, TRUE))
  expect_equal(ft_filter(x, p_usable = 0.3)$keep, c(TRUE, TRUE))
  expect_equal(ft_filter(x, "strict", stability = 0.9)$keep, c(FALSE, FALSE))
  expect_equal(ft_filter(x, "face_view", p_face = 0.7)$reason[2], "side view")
})

test_that("missing values fail the checks and text logicals are read", {
  x <- data.frame(mask_ok = c("True", "False", NA), stab = NA, p_usable = 0.9, p_face_view = 0.9)
  d <- ft_filter(x, "default")
  expect_equal(d$keep, c(TRUE, FALSE, FALSE))
  expect_false(ft_filter(x, "strict")$keep[1])
  y <- data.frame(mask_ok = TRUE, p_usable = NA)
  expect_false(ft_filter(y)$keep)
  expect_true(is.na(ft_filter(y)$view))
})

test_that("results objects print, summarise and plot", {
  res <- ft_example_results()
  expect_s3_class(res, "ft_results")
  expect_equal(nrow(res), nrow(ft_example_photos()))
  expect_true(all(c("keep", "reason", "view", "p_usable", "colour_name") %in% names(res)))
  expect_true(any(!res$keep))
  expect_output(print(res), "ft_results")
  s <- summary(res)
  expect_equal(sum(s$n), nrow(res))
  expect_equal(sum(s$n[s$kept]), sum(res$keep))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(plot(res))
})

test_that("subsetting results keeps the rule", {
  r <- ft_example_results("strict")
  s <- r[r$keep, c("photo_id", "keep", "reason")]
  expect_s3_class(s, "ft_results")
  expect_identical(attr(s, "rule"), "strict")
  expect_output(print(s), "rule: strict")
  expect_false(inherits(r[, c("photo_id", "L_mean")], "ft_results"))
  expect_type(r[, "L_mean"], "double")
})
