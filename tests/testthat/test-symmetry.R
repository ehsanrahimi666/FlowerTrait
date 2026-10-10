flower <- function(n, size = 240, depth = 0.4, zyg = 0, rot = 0.3) {
  yy <- row(matrix(0, size, size)) - size / 2
  xx <- col(matrix(0, size, size)) - size / 2
  th <- atan2(yy, xx) + rot
  rr <- sqrt(xx^2 + yy^2)
  rr < 0.3 * size * (1 + depth * cos(n * th)) * (1 + zyg * cos(th))
}

test_that("petal number is recovered for synthetic flowers", {
  for (n in c(3, 4, 5, 6, 8, 13)) {
    s <- ft_symmetry(flower(n))
    expect_equal(s$petal_n, n)
    expect_equal(s$lobe_depth, 0.4, tolerance = 0.05)
    expect_gt(s$sym_rotational, 0.95)
  }
})

test_that("bilateral flowers lose rotational but keep mirror symmetry", {
  r <- ft_symmetry(flower(5))
  z <- ft_symmetry(flower(5, zyg = 0.5))
  expect_lt(z$sym_rotational, r$sym_rotational - 0.2)
  expect_gt(z$sym_bilateral, 0.8)
  disc <- ft_symmetry(sqrt((row(matrix(0, 150, 150)) - 75)^2 + (col(matrix(0, 150, 150)) - 75)^2) < 60)
  expect_lt(disc$lobe_depth, 0.01)
  expect_gt(disc$fill_circle, 0.95)
  expect_equal(disc$aspect, 1, tolerance = 0.01)
})

test_that("geometry does not depend on resolution above the working size", {
  a <- ft_symmetry(flower(6, size = 300))
  b <- ft_symmetry(flower(6, size = 600))
  expect_equal(a$petal_n, b$petal_n)
  expect_equal(a$sym_rotational, b$sym_rotational, tolerance = 0.03)
  expect_equal(a$lobe_depth, b$lobe_depth, tolerance = 0.03)
})

test_that("tiny outlines give missing values and profiles are returned", {
  m <- matrix(FALSE, 20, 20)
  m[5:10, 5:10] <- TRUE
  expect_true(is.na(ft_symmetry(m)$petal_n))
  p <- attr(ft_symmetry(flower(5), profile = TRUE), "profile")
  expect_length(p$r, 360)
  f <- tempfile(fileext = ".png")
  ft_write_mask(flower(5), f)
  expect_equal(ft_symmetry(c(f, f))$petal_n, c(5, 5))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(ft_plot_symmetry(NULL, flower(5)))
})
