# Builds the data sets of FlowerTrait from the validation outputs:
#   ft_validation (one row per validation photo), ft_catalogue (trait reliability),
#   ft_benchmark (segmentation, photo filter, colour correction, augmentation, iNaturalist annotations)
#   and ft_reference (species-level comparison with reflectance spectra).
# Inputs are in data-raw/validation/ (written by the validation scripts in data-raw/validation/code/).
# Run from the package root: Rscript data-raw/make_datasets.R
suppressMessages(devtools::load_all(".", quiet = TRUE))
V <- "data-raw/validation"
rd <- function(f, ...) utils::read.csv(file.path(V, f), stringsAsFactors = FALSE, check.names = FALSE, ...)
has <- function(f) file.exists(file.path(V, f))
missing_inputs <- c("oxford_results.csv", "invariance_long.csv", "white_balance.csv", "augmentation.csv",
                    "annotations.csv")[!vapply(c("oxford_results.csv", "invariance_long.csv", "white_balance.csv",
                                                 "augmentation.csv", "annotations.csv"), has, logical(1))]
if (length(missing_inputs)) warning("Inputs not yet available (staged build): ", paste(missing_inputs, collapse = ", "))
set.seed(1)

# ---------------------------------------------------------------- photos, traits, labels, filter
# na.strings = "": the country code of Namibia ("NA") must not be read as missing
meta <- rd("photo_set.csv", colClasses = c(photo_id = "character", occurrenceKey = "character"), na.strings = "")
tr <- rd("traits_all.csv", colClasses = c(photo_id = "character"))
lab <- rd("labels.csv", colClasses = c(photo_id = "character"))
fp <- rd("filter_preds.csv", colClasses = c(photo_id = "character"))
tr$colour_name <- ifelse(is.finite(tr$petal_L), ft_colour_name(tr$petal_L, tr$petal_a, tr$petal_b), NA)
keep_cols <- c("photo_id", "photo_w", "photo_h", "det1_score", "n_other_flowers", "cut_by_frame", "box1_frac",
               "flower_px_native", "det_score", "sam_iou", "stab", "iou_stage1", "px", "mask_frac", "border_touch",
               "mask_ok", "solidity", "clip_hi", "clip_lo", "sharp", "D_px", .ft_core_traits, "centre_frac",
               "sym_axis_deg", "petal_power", "colour_name")
cat_traits <- c("colour_name", "dominant_name")
ext <- c("L_median", "L_p10", "L_p90", "L_range", "C_p90", "chrom_r", "chrom_g", "chrom_b", "logchrom_rg",
         "logchrom_bg", "Y_mean", "vap", "dark_frac_L30", "L_otsu", "tone_bimodality", "darkness_cw", "bg_dE00",
         "bg_weber_Y", "n_colour_classes", "colour_shannon", "colour_simpson", "colour_contrast_dE",
         "colour_max_dE", "dark_class_frac", "dark_class_L", "dominant_frac", "dominant_L", "dominant_C",
         "dominant_hue", "gran_D2", "gran_D4", "gran_D8", "gran_D16", "gran_D32", "gran_D64", "gran_total",
         "gran_slope", "gran_peak_D", "gran_peak_prop", "edge_density", "glcm_contrast", "glcm_homogeneity",
         "glcm_energy", "glcm_correlation", "pattern_fd", "land_n_patches", "land_LPI", "land_mean_patch_frac",
         "land_edge_density", "land_AI", "land_contag", "moran_I", "shape_circularity", "shape_solidity",
         "shape_aspect", "shape_roughness", "dominant_name")
# rights holders and observer names (non-ASCII) stay in data-raw/validation/photo_set.csv
meta <- meta[, setdiff(names(meta), c("recordedBy", "rightsHolder"))]
v <- merge(meta, tr[, intersect(c(keep_cols, ext, "error"), names(tr))], by = "photo_id", all.x = TRUE)
v <- merge(v, fp[, c("photo_id", grep("^p_[a-z_]+$", names(fp), value = TRUE))], by = "photo_id", all.x = TRUE)
names(lab)[names(lab) != "photo_id"] <- paste0("label_", names(lab)[names(lab) != "photo_id"])
v <- merge(v, lab, by = "photo_id", all.x = TRUE)
v$label_usable <- with(v, ifelse(is.na(label_view), NA, label_state == "O" & label_vis == "F" &
                                   label_focus == "S" & label_view != "U"))
v$outline_rating <- c(G = "good", M = "minor error", C = "several flowers", W = "wrong")[v$label_outline]
v$label_outline <- NULL
v$mask_ok <- FlowerTrait:::.ft_lgl(v$mask_ok)
v$cut_by_frame <- FlowerTrait:::.ft_lgl(v$cut_by_frame)
v <- v[order(match(v$role, c("benchmark", "pair_sibling", "pair_new", "negative")), v$region, v$species,
             v$photo_id), ]
rownames(v) <- NULL
ft_validation <- v

# ---------------------------------------------------------------- decision rules on benchmark photos
b <- v[v$role == "benchmark", ]
rules <- c("none", "qc", "default", "strict", "face_view")
rule_tab <- do.call(rbind, lapply(rules, function(r) {
  k <- ft_filter(b, r)$keep
  o <- b$outline_rating[k]
  data.frame(rule = r, kept = mean(k), good = mean(o == "good"), minor = mean(o == "minor error"),
             several = mean(o == "several flowers"), wrong = mean(o == "wrong"),
             usable = mean(b$label_usable[k]), n_kept = sum(k))
}))

# ---------------------------------------------------------------- repeatability (pairs)
pairs <- v[v$role %in% c("benchmark", "pair_sibling", "pair_new"), ]
pairs <- pairs[pairs$occurrenceKey %in% names(which(table(pairs$occurrenceKey) == 2)), ]
num_traits <- c(.ft_core_traits, setdiff(ext, cat_traits))
both <- function(rule) {
  k <- ft_filter(pairs, rule)$keep
  ok <- tapply(k, pairs$occurrenceKey, all)
  pairs[pairs$occurrenceKey %in% names(ok)[ok], ]
}
sets <- list(all = pairs, qc = {
  p <- both("qc")
  st <- tapply(p$stab >= 0.8, p$occurrenceKey, all)
  p[p$occurrenceKey %in% names(st)[st], ]
}, default = both("default"), strict = both("strict"), face = both("face_view"))
rep <- lapply(names(sets), function(s) {
  r <- ft_repeatability(sets[[s]], traits = num_traits, group = "occurrenceKey", boot = 300, seed = 1)
  names(r)[2:6] <- paste0(c("icc_", "icc_", "icc_", "n_", "n_photos_"), s, c("", "_lo", "_hi", "", ""))
  r[, 1:5]
})
rep <- Reduce(function(a, b) merge(a, b, by = "trait", sort = FALSE), rep)

# ---------------------------------------------------------------- invariance (all traits, Python long table)
inv <- if (has("invariance_long.csv")) rd("invariance_long.csv", colClasses = c(photo_id = "character")) else
  data.frame(photo_id = character(), condition = character())
conds <- c("white_balance", "exposure", "jpeg", "blur", "rotate", "perspective", "D32", "D64", "D128", "D256")
inv_tab <- data.frame(trait = num_traits)
for (cd in conds) {
  sub <- inv[inv$condition %in% c("original", cd), ]
  both_ids <- names(which(tapply(sub$condition, sub$photo_id, function(z) length(unique(z))) == 2))
  sub <- sub[sub$photo_id %in% both_ids, ]
  inv_tab[[paste0("inv_", cd)]] <- vapply(num_traits, function(t) {
    if (!t %in% names(sub) || !nrow(sub)) return(NA_real_)
    x <- suppressWarnings(as.numeric(sub[[t]]))
    if (t %in% .ft_circular) x <- FlowerTrait:::.ft_rotate(x)
    if (length(both_ids) < 10) return(NA_real_)
    ft_icc(x, sub$photo_id)
  }, numeric(1))
}
phot <- c("inv_white_balance", "inv_exposure", "inv_jpeg", "inv_blur")
geom <- c("inv_rotate", "inv_perspective")
mn <- function(z) if (all(is.na(z))) NA_real_ else min(z, na.rm = TRUE)
inv_tab$inv_photometric_min <- apply(inv_tab[, phot], 1, mn)
inv_tab$inv_geometric_min <- apply(inv_tab[, geom], 1, mn)
lad <- c(32, 64, 128, 256)
inv_tab$min_D_px <- apply(inv_tab[, paste0("inv_D", lad)], 1, function(z) {
  ok <- which(!is.na(z) & z >= 0.8)
  # smallest rung from which all larger rungs also reach 0.8
  for (i in seq_along(lad)) if (all(!is.na(z[i:length(lad)]) & z[i:length(lad)] >= 0.8)) return(lad[i])
  NA_real_
})

# ---------------------------------------------------------------- between-species share
one <- v[v$role == "benchmark", ]
one_s <- one[ft_filter(one, "strict")$keep, ]
sp_icc <- data.frame(trait = num_traits,
                     species_icc_all = vapply(num_traits, function(t) {
                       x <- as.numeric(one[[t]]); if (t %in% .ft_circular) x <- FlowerTrait:::.ft_rotate(x)
                       ft_icc(x, one$species)
                     }, numeric(1)),
                     species_icc_filtered = vapply(num_traits, function(t) {
                       x <- as.numeric(one_s[[t]]); if (t %in% .ft_circular) x <- FlowerTrait:::.ft_rotate(x)
                       ft_icc(x, one_s$species)
                     }, numeric(1)))

# ---------------------------------------------------------------- catalogue
desc <- rd("trait_descriptions.csv")
cat_df <- merge(desc, rep, by = "trait", all.x = TRUE, sort = FALSE)
cat_df <- merge(cat_df, inv_tab, by = "trait", all.x = TRUE, sort = FALSE)
cat_df <- merge(cat_df, sp_icc, by = "trait", all.x = TRUE, sort = FALSE)
cat_df <- cat_df[match(desc$trait, cat_df$trait), ]
num <- cat_df$type == "numeric"
rel <- num & !is.na(cat_df$icc_strict) & cat_df$icc_strict >= 0.6 & cat_df$inv_photometric_min >= 0.8 &
  !is.na(cat_df$min_D_px) & cat_df$min_D_px <= 128
care <- num & !rel & !is.na(cat_df$icc_strict) & cat_df$icc_strict >= 0.4 & cat_df$inv_photometric_min >= 0.7
cat_df$verdict <- ifelse(!num, "see ft_reference", ifelse(rel, "reliable", ifelse(care, "use with care", "not reliable")))
cat_df$reason <- ifelse(cat_df$verdict %in% c("reliable", "see ft_reference"), "",
  ifelse(cat_df$icc_strict < 0.6 & !is.na(cat_df$icc_strict), "low repeatability",
         ifelse(cat_df$inv_photometric_min < 0.8, "camera-sensitive",
                ifelse(is.na(cat_df$min_D_px) | cat_df$min_D_px > 128, "needs large flowers", ""))))
rownames(cat_df) <- NULL
ft_catalogue <- cat_df

# ---------------------------------------------------------------- benchmarks
seg <- if (has("oxford_results.csv")) rd("oxford_results.csv") else
  data.frame(image = character(), cls = character(), detector = character(), two_stage = logical(), segmenter = character(),
             iou = numeric(), precision = numeric(), recall = numeric(), mask_ok = logical(), sec_detect = numeric(),
             sec_segment = numeric())
seg$two_stage <- FlowerTrait:::.ft_lgl(seg$two_stage)
seg$mask_ok <- FlowerTrait:::.ft_lgl(seg$mask_ok)
seg$config <- paste(seg$detector, ifelse(seg$two_stage, "two-stage", "one-stage"), seg$segmenter)
seg_tab <- do.call(rbind, lapply(split(seg, seg$config), function(d) {
  data.frame(detector = d$detector[1], two_stage = d$two_stage[1], segmenter = d$segmenter[1], n = nrow(d),
             iou_mean = mean(d$iou), iou_median = stats::median(d$iou), share_iou_08 = mean(d$iou >= 0.8),
             precision = mean(d$precision, na.rm = TRUE), recall = mean(d$recall), mask_ok = mean(d$mask_ok),
             sec = mean(d$sec_detect + d$sec_segment))
}))
if (!is.null(seg_tab)) {
  seg_tab <- seg_tab[order(-seg_tab$iou_mean), ]
  rownames(seg_tab) <- NULL
}
filt <- rd("filter_eval.csv")
filt$used <- toupper(as.character(filt$used)) %in% "TRUE"
opt <- function(f) if (has(f)) rd(f) else data.frame()
wb <- opt("white_balance.csv")
aug <- opt("augmentation.csv")
ann <- opt("annotations.csv")
ft_benchmark <- list(segmentation = seg_tab, segmentation_images = seg[, c("image", "cls", "detector", "two_stage",
                     "segmenter", "iou", "precision", "recall", "mask_ok", "sec_detect", "sec_segment")],
                     decision_rules = rule_tab, photo_filter = filt, white_balance = wb, augmentation = aug,
                     annotations = ann)

# ---------------------------------------------------------------- reference spectra
spec <- rd("reference_spectra.csv")
wl <- 300:700
# reflectance scale (percent or fraction) is decided per data set
lab <- matrix(NA_real_, nrow(spec), 5, dimnames = list(NULL, c("L", "a", "b", "C", "h")))
for (ds in unique(spec$dataset)) {
  i <- which(spec$dataset == ds)
  lab[i, ] <- ft_spectra_to_lab(wl, t(as.matrix(spec[i, paste0("R", wl)])))
}
spec <- cbind(spec[, c("dataset", "region", "country", "species", "sample", "part", "human_colour", "symmetry")],
              lab)
ref <- do.call(rbind, lapply(split(spec, paste(spec$dataset, spec$species)), function(d) {
  data.frame(ref_species = d$species[1], dataset = d$dataset[1], region = d$region[1], n_spectra = nrow(d),
             ref_L = mean(d$L), ref_a = mean(d$a), ref_b = mean(d$b),
             human_colour = paste(sort(unique(d$human_colour[nzchar(d$human_colour)])), collapse = ";"),
             symmetry = if (any(nzchar(d$symmetry))) names(sort(table(d$symmetry[nzchar(d$symmetry)]),
                                                                 decreasing = TRUE))[1] else "")
}))
ref$ref_C <- sqrt(ref$ref_a^2 + ref$ref_b^2)
ref$ref_h <- (atan2(ref$ref_b, ref$ref_a) * 180 / pi) %% 360
ref$ref_name <- ft_colour_name(ref$ref_L, ref$ref_a, ref$ref_b)
# one photo per observation: the first photo kept by the default rule (flowering observations only)
kept <- ft_filter(v[v$role != "negative", ], "default")
kept <- kept[kept$keep, ]
kept <- kept[!duplicated(kept$occurrenceKey), ]
ph <- do.call(rbind, lapply(split(kept, kept$ref_species), function(d) {
  data.frame(ref_species = d$ref_species[1], species = d$species[1], n_photos = nrow(d),
             photo_L = stats::median(d$petal_L, na.rm = TRUE), photo_a = stats::median(d$petal_a, na.rm = TRUE),
             photo_b = stats::median(d$petal_b, na.rm = TRUE),
             photo_name_mode = names(sort(table(d$colour_name), decreasing = TRUE))[1],
             photo_name_share = max(table(d$colour_name)) / nrow(d),
             photo_sym_rot = stats::median(d$sym_rotational[d$view %in% "face"], na.rm = TRUE),
             photo_sym_bil = stats::median(d$sym_bilateral[d$view %in% "face"], na.rm = TRUE),
             n_face = sum(d$view %in% "face"))
}))
ph$photo_C <- sqrt(ph$photo_a^2 + ph$photo_b^2)
ph$photo_h <- (atan2(ph$photo_b, ph$photo_a) * 180 / pi) %% 360
ph$photo_name <- ft_colour_name(ph$photo_L, ph$photo_a, ph$photo_b)
sel <- unique(meta[, c("ref_species", "region")])
ft_reference <- merge(merge(sel, ref, by = c("ref_species", "region")), ph, by = "ref_species", all.x = TRUE)
ft_reference$dE00 <- FlowerTrait:::.ft_de00(as.matrix(ft_reference[, c("ref_L", "ref_a", "ref_b")]),
                                             as.matrix(ft_reference[, c("photo_L", "photo_a", "photo_b")]))
ft_reference <- ft_reference[order(ft_reference$region, ft_reference$species), ]
rownames(ft_reference) <- NULL

# ---------------------------------------------------------------- save
save(ft_validation, file = "data/ft_validation.rda", compress = "xz")
save(ft_catalogue, file = "data/ft_catalogue.rda", compress = "xz")
save(ft_benchmark, file = "data/ft_benchmark.rda", compress = "xz")
save(ft_reference, file = "data/ft_reference.rda", compress = "xz")
message("Saved: ", nrow(ft_validation), " photos, ", nrow(ft_catalogue), " traits, ", nrow(ft_reference),
        " reference species.")
