test_that("back end processes an example photo", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("FLOWERTRAIT_TEST_BACKEND"), "true"),
              "set FLOWERTRAIT_TEST_BACKEND=true to test the Python back end")
  skip_if_not(ft_backend_available())
  ex <- ft_example_photos()
  out <- file.path(tempdir(), "ft_backend_test")
  res <- ft_process(ex$photo[1], out_dir = out)
  expect_equal(nrow(res), 1)
  expect_true(all(c("det_score", "stab", "p_usable", "p_face_view", "L_mean", "petal_n",
                    "colour_name", "glcm_contrast") %in% names(res)))
  r <- ft_traits(file.path(out, "crops", paste0(res$photo_id, ".jpg")),
                 file.path(out, "masks", paste0(res$photo_id, ".png")))
  expect_equal(r$L_mean, res$L_mean, tolerance = 1e-3)
  expect_equal(r$petal_n, res$petal_n)
})

test_that("untested prompts warn", {
  expect_silent(FlowerTrait:::.ft_check_prompt("flower"))
  expect_warning(FlowerTrait:::.ft_check_prompt("insect"), "untested")
  expect_warning(FlowerTrait:::.ft_check_prompt(""), "untested")
})

test_that("photo inputs are turned into a table", {
  ex <- ft_example_photos()
  it <- FlowerTrait:::.ft_items(ex$photo[1:2])
  expect_equal(it$photo_id, ex$photo_id[1:2])
  it2 <- FlowerTrait:::.ft_items(dirname(ex$photo[1]))
  expect_equal(nrow(it2), nrow(ex))
  df <- data.frame(photo_id = "a", url = "https://example.org/x.jpg", family = "Papaveraceae")
  it3 <- FlowerTrait:::.ft_items(df)
  expect_equal(it3$path, "https://example.org/x.jpg")
  expect_error(FlowerTrait:::.ft_items("no_such_file.jpg"), "not found")
  expect_error(FlowerTrait:::.ft_items(data.frame(photo_id = c("a", "a"), url = c("u", "v"))),
               "unique")
  expect_error(FlowerTrait:::.ft_items(list(1)), "folder")
})
