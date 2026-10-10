# FlowerTrait 0.1.0

* First release, built on the design of InsectTrait.
* Photos from a single file, a folder, a folder of species sub-folders
  (`ft_read_folder()`, `ft_taxonomy()`), GBIF (`ft_gbif_photos()`) or the
  iNaturalist API (`ft_inat_photos()`), restricted by default to observations
  annotated as flowering and to CC0 and CC BY photos at original size; names
  that are not flowering plants are refused.
* Optional Python back end: two-stage Grounding DINO detection of the main
  flower (largest near-best box, inflorescence boxes skipped), SAM 2.1 outline
  with prompt stability, outline quality control, photo filter (usable, face
  view, open flower, whole flower, sharp) and the full trait set
  (`ft_process()`), run in a separate Python process.
* Native R computation of the core colour traits, colour names
  (`ft_colour_name()`), centre-periphery colour contrast, symmetry, petal number
  and shape (`ft_traits()`, `ft_symmetry()`, `ft_plot_symmetry()`), matching the
  back end.
* Conversion of reflectance spectra to the colour space of the traits
  (`ft_spectra_to_lab()`).
* Colour checks (`ft_colour_check()`, `ft_white_balance()`), augmentation with
  trait stability checks (`ft_augment()`, `ft_augment_check()`), decision rules
  (`ft_filter()`), trait catalogue (`ft_catalogue`), repeatability and
  invariance tools, plots (`ft_plot_colours()` and others) and summaries.
* Validation data: `ft_validation`, `ft_benchmark` (outlines against manual
  segmentations, photo filter, colour correction, augmentation, iNaturalist
  annotations) and `ft_reference` (photo colour against reflectance spectra).
