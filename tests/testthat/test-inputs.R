test_that("species folders are read and photo ids are unique", {
  d <- file.path(tempdir(), "ft_species_photos")
  unlink(d, recursive = TRUE)
  ex <- ft_example_photos()
  for (i in 1:3) {
    dir.create(file.path(d, gsub(" ", "_", ex$species[i])), recursive = TRUE, showWarnings = FALSE)
    file.copy(ex$photo[i], file.path(d, gsub(" ", "_", ex$species[i]), "IMG_001.jpg"))
  }
  file.copy(ex$photo[4], file.path(d, "loose.jpg"))
  f <- ft_read_folder(d)
  expect_equal(nrow(f), 4)
  expect_equal(sort(stats::na.omit(f$species)), sort(ex$species[1:3]))
  expect_false(anyDuplicated(f$photo_id) > 0)
  expect_true(is.na(f$species[f$file == "loose.jpg"]))
  expect_error(ft_read_folder(file.path(d, "nope")), "not found")
})

test_that("iNaturalist records are parsed", {
  js <- jsonlite::read_json(test_path("fixtures", "inat_observations.json"), simplifyVector = FALSE)
  p <- FlowerTrait:::.ft_parse_inat(js$results)
  expect_gt(nrow(p), 0)
  expect_true(all(grepl("/original\\.", p$url)))
  expect_true(all(p$license %in% c("cc0", "cc-by")))
})

test_that("live taxonomy and iNaturalist searches work", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline("api.inaturalist.org")
  tx <- ft_taxonomy(c("Papaver rhoeas", "Apis mellifera")) |> suppressWarnings()
  expect_equal(tx$family[1], "Papaveraceae")
  expect_warning(ft_taxonomy("Apis mellifera"), "flowering plant")
  p <- ft_inat_photos("Papaver rhoeas", n = 2)
  expect_lte(nrow(p), 2)
  expect_error(ft_inat_photos("Apis mellifera", n = 1), "not a flowering plant")
})
