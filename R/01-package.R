#' FlowerTrait: colour, pattern, symmetry and shape traits of flowers from photographs
#'
#' @description
#' \pkg{FlowerTrait} measures colour, pattern, symmetry and shape traits of
#' flowers in field photographs, for example the research-grade 'iNaturalist'
#' photos served by 'GBIF'. The detector prompt, the photo filter and the trait
#' catalogue were developed and validated on photographs of flowering plants
#' (angiosperms) from six continents; colour was validated against
#' spectrophotometric measurements of the same species and outlines against
#' manual segmentations.
#'
#' @section Workflow:
#' \enumerate{
#'   \item Find photos: [ft_gbif_photos()], [ft_inat_photos()] and
#'     [ft_download_photos()] (flowering observations by default), or use your
#'     own: one photo, a folder, or a folder with one sub-folder per species
#'     ([ft_read_folder()], [ft_taxonomy()]).
#'   \item Process photos with the optional 'Python' back end
#'     ([ft_install()], [ft_use_backend()], [ft_process()]): two-pass
#'     'Grounding DINO' detection of the main flower, 'SAM' 2.1 outline,
#'     quality control, photo filter and the full trait set.
#'   \item Keep usable photos with a validated decision rule: [ft_filter()].
#'   \item Check photos and traits: [ft_colour_check()], [ft_white_balance()],
#'     [ft_augment()], [ft_augment_check()], [ft_trait_info()],
#'     [ft_reliable_traits()], [ft_repeatability()], [ft_invariance()].
#'   \item Summarise and plot: [ft_summarise()], [ft_plot_traits()],
#'     [ft_plot_colours()], [ft_contact_sheet()].
#' }
#' Core colour traits, colour names, radial colour pattern, symmetry, petal
#' number and outline quality can be computed in R alone from a crop and its
#' outline with [ft_traits()] and [ft_symmetry()]. Reflectance spectra can be
#' converted to the same colour space with [ft_spectra_to_lab()].
#'
#' @section Data:
#' [ft_catalogue] (reliability of the traits), [ft_validation] (the validation
#' photos), [ft_benchmark] (segmentation benchmark against manual outlines,
#' photo filter, colour correction and augmentation) and [ft_reference]
#' (species-level comparison with reflectance spectra).
#'
#' @examples
#' ex <- ft_example_photos()
#' ft_traits(ex$crop[1], ex$mask[1])[, c("L_mean", "colour_name", "petal_n")]
#' summary(ft_example_results())
#' @keywords internal
"_PACKAGE"

.ft_core_traits <- c("L_mean", "L_sd", "C_mean", "a_mean", "b_mean", "hue_deg", "hue_conc", "vap_wp",
                     "dark_frac_otsu", "L_entropy", "centre_dL", "centre_dE", "petal_L", "petal_a", "petal_b",
                     "sym_bilateral", "sym_rotational", "petal_n", "lobe_depth", "radius_cv", "fill_circle",
                     "aspect", "solidity_shape")
.ft_circular <- c("hue_deg", "dominant_hue")

utils::globalVariables(c("ft_catalogue", "ft_validation", "ft_benchmark", "ft_reference"))
