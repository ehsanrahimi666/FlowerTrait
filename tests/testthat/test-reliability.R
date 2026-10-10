test_that("ICC(1) behaves as expected", {
  set.seed(2)
  g <- rep(1:40, each = 2)
  true <- rnorm(40)[g]
  expect_gt(ft_icc(true + rnorm(80, sd = 0.2), g), 0.9)
  expect_lt(abs(ft_icc(rnorm(80), g)), 0.35)
  expect_true(is.na(ft_icc(c(1, 2), c(1, 1))))
  expect_true(is.na(ft_icc(rep(1, 10), rep(1:5, 2))))
  # unbalanced groups and missing values
  v <- c(1, 1.1, 2, 2.2, 2.1, 3, 3.1, NA, 5)
  gg <- c(1, 1, 2, 2, 2, 3, 3, 4, 5)
  expect_gt(ft_icc(v, gg), 0.9)
})

test_that("repeatability uses circular rotation for hue and bootstraps groups", {
  set.seed(3)
  n <- 30
  base <- runif(n, 0, 360)
  d <- data.frame(occurrenceKey = rep(seq_len(n), each = 2),
                  hue_deg = (rep(base, each = 2) + rnorm(2 * n, sd = 3)) %% 360,
                  L_mean = rep(rnorm(n, 60, 10), each = 2) + rnorm(2 * n))
  r <- ft_repeatability(d, traits = c("hue_deg", "L_mean"), boot = 50)
  expect_equal(r$n_groups, c(n, n))
  expect_gt(min(r$icc), 0.8)
  expect_true(all(r$lower <= r$icc & r$icc <= r$upper))
  expect_error(ft_repeatability(d, group = "nope"), "not found")
  expect_error(ft_repeatability(d, traits = "nope"), "not found")
})

test_that("invariance runs on example crops", {
  ex <- ft_example_photos()
  inv <- ft_invariance(ex$crop[1:4], ex$mask[1:4], types = c("exposure", "jpeg"), rungs = 64, min_n = 3)
  expect_true(all(c("exposure", "jpeg") %in% names(inv)))
  expect_true("L_mean" %in% rownames(inv))
  expect_gt(inv["L_mean", "jpeg"], 0.9)
  expect_true(is.data.frame(attr(inv, "data")))
})
