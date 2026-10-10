fixture <- function(f) jsonlite::read_json(test_path("fixtures", f), simplifyVector = FALSE)

test_that("occurrence records are parsed into one row per photo", {
  js <- fixture("gbif_occurrences.json")
  p <- FlowerTrait:::.ft_parse_occurrences(js)
  expect_true(all(p$license %in% c("CC0_1_0", "CC_BY_4_0")))
  expect_true(all(grepl("^https://", p$url)))
  expect_equal(unique(p$species), "Papaver rhoeas")
  expect_true("reproductiveCondition" %in% names(p))
  empty <- FlowerTrait:::.ft_parse_occurrences(list(results = list()))
  expect_equal(nrow(empty), 0)
  expect_named(empty, names(p))
  p2 <- FlowerTrait:::.ft_parse_occurrences(js, max_per_occurrence = 2)
  expect_gte(nrow(p2), nrow(p))
})

test_that("only flowering plants are accepted", {
  expect_equal(FlowerTrait:::.ft_check_plant(fixture("gbif_match_plant.json")),
               fixture("gbif_match_plant.json")$usageKey)
  expect_error(FlowerTrait:::.ft_check_plant(fixture("gbif_match_insect.json")), "not a flowering plant")
  expect_error(FlowerTrait:::.ft_check_plant(list(matchType = "NONE")), "No GBIF")
})

test_that("licences are mapped and attribution lines are built", {
  expect_equal(FlowerTrait:::.ft_license_code(c("http://creativecommons.org/publicdomain/zero/1.0/",
                                                "http://creativecommons.org/licenses/by/4.0/",
                                                "http://creativecommons.org/licenses/by-nc/4.0/",
                                                "CC-BY", NA)),
               c("CC0_1_0", "CC_BY_4_0", "CC_BY_NC_4_0", "CC_BY_4_0", "other"))
  a <- ft_attribution(ft_example_photos())
  expect_length(a, nrow(ft_example_photos()))
  expect_true(all(grepl("Photo: .+ \\((CC0 1.0|CC BY 4.0)\\)", a)))
})

test_that("live GBIF search returns flowering observations", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline("api.gbif.org")
  p <- ft_gbif_photos("Papaver rhoeas", n = 3)
  expect_lte(nrow(p), 3)
  expect_true(all(p$license %in% c("CC0_1_0", "CC_BY_4_0")))
  expect_true(all(grepl("flowers", p$reproductiveCondition)))
  expect_error(ft_gbif_photos("Apis mellifera", n = 1), "not a flowering plant")
})
