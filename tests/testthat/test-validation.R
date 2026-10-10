test_that("decision rules reproduce the validation", {
  n <- vapply(c("qc", "default", "strict", "face_view"),
              function(r) sum(ft_filter(ft_validation, r)$keep), numeric(1))
  expect_equal(unname(n), c(737, 570, 486, 282))
  b <- ft_validation[ft_validation$role == "benchmark", ]
  expect_equal(nrow(b), 487)
  kept <- vapply(c("default", "strict", "face_view"), function(r) mean(ft_filter(b, r)$keep), numeric(1))
  expect_equal(unname(round(kept, 3)), c(0.747, 0.643, 0.392))
  k <- ft_filter(b, "default")$keep
  expect_equal(round(mean(b$outline_rating[k] == "wrong"), 3), 0.011)
  expect_equal(round(mean(b$label_usable), 3), 0.725)
})

test_that("pairs give the repeatability of the catalogue", {
  v <- ft_validation
  p <- v[v$role %in% c("benchmark", "pair_sibling", "pair_new"), ]
  p <- p[p$occurrenceKey %in% names(which(table(p$occurrenceKey) == 2)), ]
  expect_equal(length(unique(p$occurrenceKey)), 202)
  k <- tapply(ft_filter(p, "default")$keep, p$occurrenceKey, all)
  pd <- p[p$occurrenceKey %in% names(k)[k], ]
  r <- ft_repeatability(pd, traits = "L_mean", group = "occurrenceKey", boot = 0)
  expect_equal(r$icc, ft_catalogue$icc_default[ft_catalogue$trait == "L_mean"], tolerance = 1e-8)
})

test_that("country codes are read literally (Namibia is NA)", {
  expect_false(anyNA(ft_validation$country))
  expect_true("NA" %in% ft_validation$country)
  expect_equal(length(unique(ft_validation$country)), 26)
})
