# FlowerTrait validation script (provenance record). Paths refer to the working folder of the validation
# (WORK, environment variable FT_WORK) and to the package source (PKG, environment variable FT_PKG).
# Agreement between ft_traits() in R and the Python back end on the same crops and outlines.
# Rscript r_py_agree.R PYTHON_CSV CROP_FOLDER OUT_CSV
WORK <- Sys.getenv("FT_WORK", "work")  # working folder of the validation
PKG <- Sys.getenv("FT_PKG", ".")  # package source
args <- commandArgs(trailingOnly = TRUE)
suppressMessages(devtools::load_all(PKG, quiet = TRUE))
py <- utils::read.csv(args[1], colClasses = c(photo_id = "character"))
root <- args[2]
r <- ft_traits(file.path(root, "crops", paste0(py$photo_id, ".jpg")), file.path(root, "masks", paste0(py$photo_id, ".png")))
stopifnot(identical(r$photo_id, py$photo_id))
cols <- intersect(c("px", "border_touch", "solidity", "clip_hi", "clip_lo", "sharp", "D_px", .ft_core_traits), names(py))
ag <- do.call(rbind, lapply(cols, function(k) {
  a <- as.numeric(r[[k]]); b <- as.numeric(py[[k]])
  ok <- is.finite(a) & is.finite(b)
  d <- abs(a - b)
  if (k == "hue_deg") d <- pmin(d, 360 - d)
  data.frame(trait = k, n = sum(ok), identical = mean(d[ok] == 0), max_abs = max(d[ok]),
             max_rel = max(d[ok] / pmax(abs(b[ok]), 1e-9)), sd = stats::sd(b[ok]),
             na_mismatch = sum(xor(is.finite(a), is.finite(b))))
}))
utils::write.csv(ag, args[3], row.names = FALSE)
print(ag, digits = 3)
