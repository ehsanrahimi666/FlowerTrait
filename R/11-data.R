#' Trait catalogue: definitions and reliability of the flower traits
#'
#' Definitions and validation results for the core traits (computed in R by
#' [ft_traits()] and by the back end) and the extended traits (back end only),
#' from research-grade 'iNaturalist' photographs of flowering plants from six
#' continents (October 2026). Repeatability is the ICC(1) between two photos of
#' the same plant (pairs of photos from one observation), with 95% bootstrap
#' intervals (300 resamples of observations), for all pairs and for pairs in
#' which both photos passed a decision rule. Invariance is the ICC between the
#' original crop and a changed copy with the outline fixed (benchmark photos
#' that passed the strict rule).
#'
#' Verdict for numeric traits: `"reliable"` if `icc_strict >= 0.6`,
#' `inv_photometric_min >= 0.8` and `min_D_px <= 128`; `"use with care"` if
#' `icc_strict >= 0.4` and `inv_photometric_min >= 0.7`; otherwise
#' `"not reliable"`. The two categorical colour names are validated against
#' reflectance spectra and the names given by the authors of the spectra
#' ([ft_reference]).
#'
#' Pairs of photos come from the same observation (usually taken minutes apart
#' under the same light), so repeatability across days, observers and cameras
#' will be lower.
#'
#' @format A data frame with one row per trait:
#' \describe{
#'   \item{trait, family, type, description}{Trait name, family (colour,
#'     pattern, symmetry, shape), type (numeric or categorical) and definition.
#'     D is the flower's equivalent diameter; the petal periphery and the centre
#'     are defined by the relative radius (see [ft_traits()]).}
#'   \item{computed_by}{`"R and Python"` for traits that [ft_traits()] also
#'     computes, `"Python"` for back-end traits.}
#'   \item{icc_all, icc_all_lo, icc_all_hi, n_all}{Repeatability (ICC), 95%
#'     interval and number of pairs, all pairs.}
#'   \item{icc_qc, icc_qc_lo, icc_qc_hi, n_qc}{The same for pairs passing
#'     outline QC with stability >= 0.8.}
#'   \item{icc_default, icc_default_lo, icc_default_hi, n_default}{The same
#'     for pairs passing the default rule of [ft_filter()].}
#'   \item{icc_strict, icc_strict_lo, icc_strict_hi, n_strict}{The same for
#'     the strict rule; `icc_strict` is used for the verdict.}
#'   \item{icc_face, icc_face_lo, icc_face_hi, n_face}{The same for the
#'     face-view rule.}
#'   \item{inv_white_balance, inv_exposure, inv_jpeg, inv_blur, inv_rotate,
#'     inv_perspective}{Invariance ICC for each simulated change.}
#'   \item{inv_D32, inv_D64, inv_D128, inv_D256}{ICC between the original and a
#'     copy reduced so that the flower is 32-256 px across.}
#'   \item{inv_photometric_min, inv_geometric_min}{Smallest photometric
#'     (white balance, exposure, JPEG, blur) and geometric (rotation,
#'     perspective) invariance.}
#'   \item{min_D_px}{Smallest flower diameter on the ladder from which the ICC
#'     stays >= 0.8 (`NA`: not reached at 256 px).}
#'   \item{species_icc_all, species_icc_filtered}{Share of variance among
#'     species (ICC(1) with species as groups, one photo per observation), for
#'     all benchmark photos and for photos passing the strict rule.}
#'   \item{verdict, reason}{Reliability verdict and, if not reliable, why.}
#' }
#' @source Validation of the \pkg{FlowerTrait} pipeline; script
#'   `data-raw/make_datasets.R` in the package source.
#' @examples
#' table(ft_catalogue$family, ft_catalogue$verdict)
#' ft_catalogue[ft_catalogue$verdict == "not reliable", c("trait", "reason")]
#' @seealso [ft_trait_info()], [ft_reliable_traits()]
"ft_catalogue"

#' Validation photos: quality metrics, photo filter, labels and traits
#'
#' One row per validation photo: research-grade 'iNaturalist' photographs from
#' GBIF of flowering plants of species with published reflectance spectra, from
#' six regions (Europe, Middle East, North America, South America, southern
#' Africa and Australia), processed by the back end on a CPU in October 2026.
#' The table holds no images; photos can be found from `occurrenceKey` and
#' `url`. Every photo is CC0 or CC BY; rights holders are listed in
#' `data-raw/validation/photo_set.csv` of the source repository and on the
#' occurrence pages (`gbif_url`).
#'
#' `role`: `"benchmark"` (seven flowering observations per species, first photo),
#' `"pair_sibling"` (second photo of two benchmark observations per species),
#' `"pair_new"` (two photos of one more flowering observation per species) and
#' `"negative"` (one observation per species annotated by iNaturalist observers
#' as not flowering). Observations with two photos form the pairs for
#' repeatability.
#'
#' Benchmark photos carry visual labels, and their outlines are rated as good,
#' minor error, several flowers (more than one flower outlined together) or
#' wrong; the labelling procedure is described in the package article. Label
#' codes: `label_view` F (face), O (oblique), S (side), B (back or below), U
#' (unclear); `label_state` O (open), B (bud), W (wilted), X (no flower);
#' `label_vis` F (whole flower visible), P (partly hidden or cut off), H (mostly
#' hidden); `label_focus` S (sharp), B (blurred); `label_petals` visible petals,
#' lobes or ray florets of face views; `label_usable` an open, whole, sharp
#' flower in a clear view.
#'
#' @format A data frame with 802 rows (one per photo) and 140 columns:
#' \describe{
#'   \item{photo_id, occurrenceKey, role, region, species, ref_species, family,
#'     order, country, year, eventDate, decimalLatitude, decimalLongitude,
#'     reproductiveCondition, url, license, references, gbif_url}{Photo and
#'     occurrence identifiers, taxonomy (GBIF backbone; the
#'     name used by the reference spectra in `ref_species`), location,
#'     iNaturalist phenology annotation and photo licence.}
#'   \item{photo_w, photo_h}{Photo size used (longer side at most 2048 px).}
#'   \item{det1_score, n_other_flowers, cut_by_frame, box1_frac,
#'     flower_px_native}{Stage-1 detection score, number of other flowers
#'     detected with a score of 0.30 or more, whether the flower box touches the
#'     photo frame, box area relative to the photo and longer side of the box.}
#'   \item{det_score, sam_iou, stab, iou_stage1}{Stage-2 detection score, SAM
#'     predicted IoU, outline stability (smallest IoU with the jittered-box
#'     outlines) and IoU with the outline from the stage-1 box.}
#'   \item{px, mask_frac, border_touch, mask_ok, solidity, clip_hi, clip_lo,
#'     sharp, D_px}{Outline quality metrics and equivalent diameter (see
#'     [ft_traits()]).}
#'   \item{p_usable, p_face_view, p_open_flower, p_fully_visible, p_sharp}{
#'     Photo-filter probabilities: out-of-fold for benchmark photos and their
#'     second photos, from the heads fitted on all labelled photos otherwise.}
#'   \item{p_usable_full, p_face_view_full, p_open_flower_full,
#'     p_fully_visible_full, p_sharp_full}{Probabilities from the heads fitted on
#'     all labelled photos (as shipped with the package).}
#'   \item{label_view, label_state, label_vis, label_focus, label_petals,
#'     label_usable, outline_rating}{Visual labels (benchmark photos only).}
#'   \item{L_mean, L_sd, C_mean, a_mean, b_mean, hue_deg, hue_conc, vap_wp,
#'     dark_frac_otsu, L_entropy, centre_dL, centre_dE, petal_L, petal_a,
#'     petal_b, sym_bilateral, sym_rotational, petal_n, lobe_depth,
#'     radius_cv, fill_circle, aspect, solidity_shape, colour_name}{Core traits computed by the back end (see
#'     [ft_traits()] and [ft_catalogue]); `colour_name` names the petal colour.}
#'   \item{centre_frac, sym_axis_deg, petal_power}{Auxiliary outputs of the radial and symmetry analysis:
#'     share of the outline in the centre zone, angle of the best mirror axis
#'     and share of the radius spectrum in the petal harmonic.}
#'   \item{L_median, L_p10, L_p90, L_range, C_p90, chrom_r, chrom_g, chrom_b,
#'     logchrom_rg, logchrom_bg, Y_mean, vap, dark_frac_L30, L_otsu,
#'     tone_bimodality, darkness_cw, bg_dE00, bg_weber_Y, n_colour_classes,
#'     colour_shannon, colour_simpson, colour_contrast_dE, colour_max_dE,
#'     dark_class_frac, dark_class_L, dominant_frac, dominant_L, dominant_C,
#'     dominant_hue, dominant_name, gran_D2, gran_D4, gran_D8, gran_D16,
#'     gran_D32, gran_D64, gran_total, gran_slope, gran_peak_D,
#'     gran_peak_prop, edge_density, glcm_contrast, glcm_homogeneity,
#'     glcm_energy, glcm_correlation, pattern_fd, land_n_patches, land_LPI,
#'     land_mean_patch_frac, land_edge_density, land_AI, land_contag,
#'     moran_I, shape_circularity, shape_solidity, shape_aspect,
#'     shape_roughness}{Extended traits of the back end (see [ft_catalogue]).}
#'   \item{error}{Processing error, empty if none.}
#' }
#' @source Validation of the \pkg{FlowerTrait} pipeline; script
#'   `data-raw/make_datasets.R`. Occurrence data: GBIF occurrence download
#'   listed in the package article (iNaturalist research-grade observations,
#'   \doi{10.15468/ab3s5x}).
#' @examples
#' table(ft_validation$region, ft_validation$role)
#' # wrong outlines among benchmark photos kept by each rule
#' b <- ft_validation[ft_validation$role == "benchmark", ]
#' sapply(c("qc", "default", "strict"), function(r) {
#'   k <- ft_filter(b, r)$keep
#'   c(kept = mean(k), wrong = mean(b$outline_rating[k] == "wrong"))
#' })
"ft_validation"

#' Benchmarks of detectors and segmenters, the photo filter, colour correction and augmentation
#'
#' `segmentation` and `segmentation_images`: automatic outlines compared with
#' the ground-truth segmentations (trimaps) of the Oxford 17 Category Flower
#' Dataset (Nilsback and Zisserman), for detection settings (Grounding DINO base
#' in two stages or one stage, Grounding DINO tiny, OWLv2 base, or no detector)
#' and segmenters (SAM 2.1 base, small and tiny, GrabCut), on one CPU core. Each
#' outline is compared with the ground-truth flower region it overlaps most,
#' excluding the unlabelled boundary band: `iou`, `precision` (share of outline
#' pixels labelled flower) and `recall`.
#'
#' `decision_rules`: share of benchmark photos kept by each rule of
#' [ft_filter()] and shares of outline ratings and usable labels among them.
#'
#' `photo_filter`: cross-validated performance of the photo-filter heads
#' against the visual labels of the benchmark photos (stratified 5-fold
#' cross-validation repeated twice; L2 logistic regression with the penalty
#' chosen by inner 3-fold cross-validation), for six feature sets and zero-shot
#' baselines.
#'
#' `white_balance`: repeatability (ICC(1)) of the core traits between the two
#' photos of the pairs that passed the default rule after each colour
#' correction of [ft_white_balance()], with the difference from uncorrected
#' traits and its 95% paired bootstrap interval.
#'
#' `augmentation`: change of the core traits under the augmentations of
#' [ft_augment()], from [ft_augment_check()] on benchmark photos that passed the
#' strict rule (one random draw per augmentation).
#'
#' `annotations`: the open-flower and usable probabilities of the photo filter
#' against the iNaturalist annotations (flowering against not flowering), which
#' observers assigned independently of the visual labels.
#'
#' @format A list of seven data frames.
#' @source Validation of the \pkg{FlowerTrait} pipeline; script
#'   `data-raw/make_datasets.R`.
#' @examples
#' ft_benchmark$segmentation[, c("detector", "two_stage", "segmenter", "iou_mean", "sec")]
#' ft_benchmark$decision_rules
#' ft_benchmark$photo_filter[ft_benchmark$photo_filter$used, ]
#' @references Nilsback, M.-E. and Zisserman, A. (2006) A visual vocabulary for
#'   flower classification. Proceedings of the IEEE Conference on Computer
#'   Vision and Pattern Recognition, 1447-1454. \doi{10.1109/CVPR.2006.42}
"ft_benchmark"

#' Flower colour in photographs compared with reflectance spectra
#'
#' One row per validation species: the colour of its flowers computed from
#' published reflectance spectra and from photographs. Spectra (300-700 nm)
#' come from the Floral Reflectance Database (FReD; Arnold et al. 2010) and six
#' data sets on Dryad (Kent Island, Canada; serpentine seeps, California;
#' Namaqualand daisies and *Erica*, South Africa; insect- and bird-pollinated
#' flowers, Australia), and were converted to CIELAB (D65, CIE 1931 2-degree
#' observer) with [ft_spectra_to_lab()] and averaged per species. Photo values
#' are medians over the photos of different observations kept by the default
#' rule. Photos and spectra come from different plants, so the comparison
#' includes variation within species.
#'
#' @format A data frame with columns `ref_species`, `region`, `dataset`,
#'   `n_spectra`, `ref_L`, `ref_a`, `ref_b`, `ref_C`, `ref_h`, `ref_name`
#'   (colour name of the spectra), `human_colour` (colour named by the authors of
#'   FReD records), `symmetry` (FReD: radial or bilateral), `species` (GBIF
#'   name), `n_photos`, `photo_L`, `photo_a`, `photo_b`, `photo_C`, `photo_h`,
#'   `photo_name` (colour name of the median photo colour), `photo_name_mode` and
#'   `photo_name_share` (most frequent name among photos and its share),
#'   `photo_sym_rot`, `photo_sym_bil` and `n_face` (median symmetry of face
#'   views and their number) and `dE00` (CIEDE2000 difference between the two
#'   colours).
#' @source Script `data-raw/make_datasets.R`. Arnold, S. E. J., Faruq, S.,
#'   Savolainen, V., McOwan, P. W. and Chittka, L. (2010) FReD: the floral
#'   reflectance database. PLoS ONE 5, e14287. \doi{10.1371/journal.pone.0014287}
#' @examples
#' r <- ft_reference
#' cor(r$ref_L, r$photo_L, method = "spearman", use = "complete.obs")
#' table(spectra = r$ref_name, photos = r$photo_name)
"ft_reference"
