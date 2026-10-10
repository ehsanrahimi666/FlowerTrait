test_that("catalogue has the validated verdicts", {
  expect_equal(nrow(ft_catalogue), 81)
  num <- ft_catalogue[ft_catalogue$type == "numeric", ]
  expect_equal(nrow(num), 79)
  expect_equal(as.vector(table(factor(num$verdict, c("reliable", "use with care", "not reliable")))),
               c(62, 12, 5))
  expect_equal(as.vector(table(ft_catalogue$family)[c("colour", "pattern", "shape", "symmetry")]),
               c(43, 26, 8, 4))
  rel <- num$verdict == "reliable"
  expect_true(all(num$icc_strict[rel] >= 0.6))
  expect_true(all(num$inv_photometric_min[rel] >= 0.8))
  expect_true(all(num$min_D_px[rel] <= 128))
  expect_true(all(ft_catalogue$trait %in% names(ft_validation)))
  expect_equal(ft_catalogue$verdict[ft_catalogue$type == "categorical"], rep("see ft_reference", 2))
})

test_that("trait lookups work", {
  expect_equal(as.character(ft_trait_info("hue_conc")$verdict), "not reliable")
  expect_equal(as.character(ft_trait_info("petal_n")$verdict), "use with care")
  expect_equal(nrow(ft_trait_info(verdict = "not reliable")), 5)
  expect_error(ft_trait_info("nope"), "Unknown")
  expect_length(ft_reliable_traits(), 62)
  expect_false("hue_conc" %in% ft_reliable_traits())
  expect_setequal(ft_reliable_traits(family = "shape"),
                  c("radius_cv", "fill_circle", "aspect", "solidity_shape", "shape_circularity",
                    "shape_solidity", "shape_aspect", "shape_roughness"))
})

test_that("benchmark tables are complete", {
  expect_named(ft_benchmark, c("segmentation", "segmentation_images", "decision_rules", "photo_filter",
                               "white_balance", "augmentation", "annotations"))
  wb <- ft_benchmark$white_balance
  expect_equal(nrow(wb), 90)
  expect_true(all(wb$diff[wb$method == "none"] == 0))
  expect_true(all(wb$icc > 0 & wb$icc <= 1))
  expect_true(all(wb$n_pairs == 119))
  expect_true(all(ft_benchmark$augmentation$median_rel_change >= 0))
  expect_equal(max(ft_benchmark$augmentation$median_rel_change[ft_benchmark$augmentation$type == "rotate90"]), 0,
               tolerance = 1e-3)
  pf <- ft_benchmark$photo_filter
  expect_equal(sum(pf$used), 5)
  expect_equal(round(pf$auc[pf$used & pf$question == "usable"], 3), 0.934)
  expect_equal(nrow(ft_benchmark$decision_rules), 5)
  expect_equal(round(ft_benchmark$annotations$auc[1], 2), 0.86)
  seg <- ft_benchmark$segmentation
  expect_true(all(seg$iou_mean >= 0 & seg$iou_mean <= 1))
  expect_true(all(c("gdino_b", "owlv2_b", "none") %in% seg$detector))
})

test_that("reference comparison holds the validation species", {
  expect_equal(nrow(ft_reference), 70)
  r <- ft_reference[!is.na(ft_reference$photo_L), ]
  expect_equal(nrow(r), 69)
  expect_equal(round(stats::cor(r$ref_b, r$photo_b, method = "spearman"), 2), 0.90)
  expect_true(all(r$ref_name %in% c("white", "grey", "black", "red", "pink", "orange", "brown", "yellow",
                                    "green", "blue", "purple")))
  expect_true(all(r$dE00 >= 0))
})
