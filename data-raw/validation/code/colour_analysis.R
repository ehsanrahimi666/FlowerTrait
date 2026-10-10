# Colour correction, augmentation and iNaturalist-annotation analyses of the validation photos.
# Needs the crops and outlines of the validation run (CROPS folder) and data/ft_validation.rda.
# Writes data-raw/validation/white_balance.csv, white_balance_reference.csv, augmentation.csv and annotations.csv.
WORK <- Sys.getenv("FT_WORK", "work")  # working folder of the validation
PKG <- Sys.getenv("FT_PKG", ".")  # package source
suppressMessages(devtools::load_all(".", quiet = TRUE))
CROPS <- Sys.getenv("FT_CROPS", file.path(WORK, "val_all"))
V <- "data-raw/validation"
v <- ft_validation
crop <- function(id) file.path(CROPS, "crops", paste0(id, ".jpg"))
mask <- function(id) file.path(CROPS, "masks", paste0(id, ".png"))
# colour and radial-pattern traits (outline geometry does not change with colour correction)
tr <- c("L_mean", "L_sd", "C_mean", "a_mean", "b_mean", "hue_deg", "hue_conc", "vap_wp", "dark_frac_otsu",
        "L_entropy", "centre_dL", "centre_dE", "petal_L", "petal_a", "petal_b")
methods <- c("none", "gray_world", "white_patch", "shades_of_gray", "gray_pixels")

# ---------------------------------------------------------------- traits after each correction (all processed crops)
# photos used below: pairs in which both photos pass the default rule, and photos kept for the spectra comparison
pp <- v[v$role %in% c("benchmark", "pair_sibling", "pair_new"), ]
pp <- pp[pp$occurrenceKey %in% names(which(table(pp$occurrenceKey) == 2)), ]
okp <- tapply(ft_filter(pp, "default")$keep, pp$occurrenceKey, all)
first_kept <- function(x) {  # one photo per observation: the first photo kept by the default rule
  k <- ft_filter(x[x$role != "negative", ], "default")
  k <- k[k$keep, ]
  k[!duplicated(k$occurrenceKey), ]
}
need <- c(pp$photo_id[pp$occurrenceKey %in% names(okp)[okp]], first_kept(v)$photo_id)
ids <- v$photo_id[v$photo_id %in% need & file.exists(crop(v$photo_id)) & v$mask_ok %in% TRUE]
cc <- ft_colour_check(crop(ids), mask(ids))
corr <- lapply(methods, function(m) {
  rows <- lapply(ids, function(i) {
    img <- ft_read_image(crop(i))
    msk <- ft_read_mask(mask(i))
    if (m != "none") img <- ft_white_balance(img, m, mask = msk, use = "background")
    cbind(photo_id = i, ft_traits(img, msk), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$method <- m
  out
})
names(corr) <- methods
cast_ids <- cc$photo_id[!is.na(cc$cast) & cc$cast > 0.2]
gp_if <- corr$none
sw <- gp_if$photo_id %in% cast_ids
gp_if[sw, ] <- corr$gray_pixels[match(gp_if$photo_id[sw], corr$gray_pixels$photo_id), ]
gp_if$method <- "gray_pixels_if_cast"
corr$gray_pixels_if_cast <- gp_if

skip_wb <- nzchar(Sys.getenv("FT_SKIP_WB"))  # set to redo only the comparison with spectra and the annotations
# ---------------------------------------------------------------- repeatability of pairs passing the default rule
p <- v[v$role %in% c("benchmark", "pair_sibling", "pair_new"), ]
p <- p[p$occurrenceKey %in% names(which(table(p$occurrenceKey) == 2)), ]
k <- ft_filter(p, "default")$keep
ok <- tapply(k, p$occurrenceKey, all)
p <- p[p$occurrenceKey %in% names(ok)[ok] & p$photo_id %in% ids, ]
p <- p[p$occurrenceKey %in% names(which(table(p$occurrenceKey) == 2)), ]
grp <- p$occurrenceKey
sdv <- vapply(tr, function(t) stats::sd(corr$none[[t]], na.rm = TRUE), numeric(1))
sdv["hue_deg"] <- {
  r <- corr$none$hue_deg[is.finite(corr$none$hue_deg)] * pi / 180
  sqrt(-2 * log(sqrt(mean(sin(r))^2 + mean(cos(r))^2))) * 180 / pi
}
icc_of <- function(d, t, g) {
  x <- d[[t]]
  if (t %in% .ft_circular) x <- FlowerTrait:::.ft_rotate(x)
  ft_icc(x, g)
}
set.seed(1)
B <- if (skip_wb) 2 else 1000
obs <- unique(grp)
rows_of <- split(seq_along(grp), factor(grp, levels = obs))
boot_idx <- replicate(B, {
  s <- sample(length(obs), replace = TRUE)
  list(rows = unlist(rows_of[s], use.names = FALSE), g = rep(seq_along(s), lengths(rows_of[s])))
}, simplify = FALSE)
wb <- do.call(rbind, lapply(names(corr), function(m) {
  d <- corr[[m]][match(p$photo_id, corr[[m]]$photo_id), ]
  d0 <- corr$none[match(p$photo_id, corr$none$photo_id), ]
  do.call(rbind, lapply(tr, function(t) {
    i1 <- icc_of(d, t, grp)
    i0 <- icc_of(d0, t, grp)
    bs <- vapply(boot_idx, function(s) icc_of(d[s$rows, ], t, s$g) - icc_of(d0[s$rows, ], t, s$g), numeric(1))
    ch <- abs(corr[[m]][[t]] - corr$none[[t]][match(corr[[m]]$photo_id, corr$none$photo_id)])
    if (t == "hue_deg") ch <- pmin(ch, 360 - ch)
    data.frame(method = m, trait = t, icc = i1, diff = i1 - i0,
               lower = stats::quantile(bs, 0.025, na.rm = TRUE, names = FALSE),
               upper = stats::quantile(bs, 0.975, na.rm = TRUE, names = FALSE),
               n_pairs = length(unique(grp)), change = stats::median(ch, na.rm = TRUE) / sdv[[t]])
  }))
}))
if (!skip_wb) utils::write.csv(wb, file.path(V, "white_balance.csv"), row.names = FALSE)

# ---------------------------------------------------------------- agreement with reflectance spectra after correction
ref <- ft_reference
wbr <- do.call(rbind, lapply(names(corr), function(m) {
  kept <- first_kept(v)
  d <- corr[[m]][match(kept$photo_id, corr[[m]]$photo_id), ]
  d$ref_species <- kept$ref_species
  s <- do.call(rbind, lapply(split(d, d$ref_species), function(z) data.frame(
    ref_species = z$ref_species[1], L = stats::median(z$petal_L, na.rm = TRUE),
    a = stats::median(z$petal_a, na.rm = TRUE), b = stats::median(z$petal_b, na.rm = TRUE))))
  s <- merge(s, ref[, c("ref_species", "ref_L", "ref_a", "ref_b")], by = "ref_species")
  s <- s[stats::complete.cases(s), ]
  de <- FlowerTrait:::.ft_de00(as.matrix(s[, c("ref_L", "ref_a", "ref_b")]), as.matrix(s[, c("L", "a", "b")]))
  data.frame(method = m, n_species = nrow(s), rho_L = stats::cor(s$L, s$ref_L, method = "spearman"),
             rho_a = stats::cor(s$a, s$ref_a, method = "spearman"),
             rho_b = stats::cor(s$b, s$ref_b, method = "spearman"),
             median_dE00 = stats::median(de),
             name_agreement = mean(ft_colour_name(s$L, s$a, s$b) == ft_colour_name(s$ref_L, s$ref_a, s$ref_b)))
}))
utils::write.csv(wbr, file.path(V, "white_balance_reference.csv"), row.names = FALSE)

# ---------------------------------------------------------------- augmentation check
if (!skip_wb) {
b <- v[v$role == "benchmark", ]
bs_ <- ft_filter(b, "strict")
sel <- bs_$photo_id[bs_$keep & file.exists(crop(bs_$photo_id))]
set.seed(2)
sel <- sample(sel, min(100, length(sel)))
ac <- ft_augment_check(crop(sel), mask(sel), seed = 1)
aug <- do.call(rbind, lapply(split(ac, paste(ac$type, ac$trait)), function(d) data.frame(
  type = d$type[1], trait = d$trait[1], median_rel_change = stats::median(d$rel_change, na.rm = TRUE),
  q90_rel_change = stats::quantile(d$rel_change, 0.9, na.rm = TRUE, names = FALSE),
  share_flagged = mean(d$flag, na.rm = TRUE), n_photos = sum(!is.na(d$rel_change)))))
rownames(aug) <- NULL
utils::write.csv(aug, file.path(V, "augmentation.csv"), row.names = FALSE)
}

# ---------------------------------------------------------------- iNaturalist annotations
neg <- v[v$role == "negative", ]
pos <- v[v$role == "benchmark", ]
auc <- function(score, y) {
  ok <- is.finite(score) & !is.na(y)
  s <- score[ok]; y <- y[ok]
  r <- rank(s)
  (sum(r[y]) - sum(y) * (sum(y) + 1) / 2) / (sum(y) * sum(!y))
}
both <- rbind(transform(pos, flowering = TRUE), transform(neg, flowering = FALSE))
ann <- data.frame(
  measure = c("p_open_flower", "p_usable"),
  auc = c(auc(both$p_open_flower, both$flowering), auc(both$p_usable, both$flowering)),
  n_flowering = sum(both$flowering), n_not_flowering = sum(!both$flowering),
  kept_default_flowering = mean(ft_filter(pos, "default")$keep),
  kept_default_not_flowering = mean(ft_filter(neg, "default")$keep))
neg_lab <- table(neg$reproductiveCondition)
ann$not_flowering_annotations <- paste(names(neg_lab), neg_lab, sep = ": ", collapse = "; ")
utils::write.csv(ann, file.path(V, "annotations.csv"), row.names = FALSE)
message("colour analyses done: ", length(ids), " crops, ", length(unique(grp)), " pairs")
