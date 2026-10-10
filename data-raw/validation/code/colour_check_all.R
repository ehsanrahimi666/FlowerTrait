# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
WORK <- Sys.getenv("FT_WORK", "work")  # working folder of the validation
PKG <- Sys.getenv("FT_PKG", ".")  # package source
suppressMessages(devtools::load_all(PKG, quiet = TRUE))
v <- ft_validation
ids <- v$photo_id[file.exists(file.path(WORK, "val_all/crops", paste0(v$photo_id, ".jpg"))) & v$mask_ok %in% TRUE]
cc <- ft_colour_check(file.path(WORK, "val_all/crops", paste0(ids, ".jpg")),
                      file.path(WORK, "val_all/masks", paste0(ids, ".png")))
utils::write.csv(cc, file.path(PKG, "data-raw/validation/colour_check.csv"), row.names = FALSE)
message("done ", nrow(cc))
