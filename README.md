# FlowerTrait

[![R-CMD-check](https://github.com/ehsanrahimi666/FlowerTrait/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ehsanrahimi666/FlowerTrait/actions/workflows/R-CMD-check.yaml)

**Colour, pattern, symmetry and shape traits of flowers from photographs**

`FlowerTrait` measures the colour, pattern, symmetry and shape of flowers in
field photographs. It finds the main flower, outlines it, decides whether the
photo can be used (open flower, whole, sharp, seen face-on or from the side),
checks colour and exposure, and returns one row of traits per photo with quality
metrics and a catalogue of how reliable each trait is. Photos can come from your
own folders or from flowering observations on GBIF and iNaturalist.

**Flowers of angiosperms.** The detector prompt, photo filter and trait
catalogue were developed and validated on 802 iNaturalist photographs of 70
species of flowering plants from six continents, against manual flower outlines
(Oxford 17 Category Flower Dataset), reflectance spectra of the same species and
the phenology annotations of iNaturalist observers. `ft_gbif_photos()`,
`ft_inat_photos()` and `ft_taxonomy()` refuse or flag names that are not
flowering plants.

## Installation

```r
install.packages("remotes")
remotes::install_github("ehsanrahimi666/FlowerTrait")
```

Most functions run in R alone. Detection, outlining, the photo filter and the
extended traits need the optional Python back end, installed once:

```r
library(FlowerTrait)
ft_install()                 # or ft_install(cpu_only = TRUE) on a laptop
ft_use_backend()             # at the start of each R session
ft_backend_available()       # TRUE when ready
```

The first run downloads the model weights (about 1.5 GB). The back end runs in
a separate Python process, so PyTorch is never loaded into R.

## Where the photos come from

```r
# one photo
res <- ft_process("PATH_TO_ONE_PHOTO.jpg", out_dir = "PATH_TO_OUTPUT_FOLDER")

# a folder of photos (searched recursively)
res <- ft_process("PATH_TO_PHOTO_FOLDER", out_dir = "PATH_TO_OUTPUT_FOLDER")

# a folder with one sub-folder per species, e.g. photos/Papaver_rhoeas/*.jpg
ph  <- ft_read_folder("PATH_TO_SPECIES_FOLDERS")   # species from folder names
ph  <- ft_taxonomy(ph)                             # adds family and order (GBIF)
res <- ft_process(ph, out_dir = "PATH_TO_OUTPUT_FOLDER")

# GBIF or iNaturalist: flowering observations, CC0 and CC BY photos, original size
ph  <- ft_gbif_photos("Papaver rhoeas", n = 100, country = "DE")
ph  <- ft_inat_photos("Papaver rhoeas", n = 100)
ph  <- ft_download_photos(ph, dir = "PATH_TO_PHOTO_FOLDER")
res <- ft_process(ph, out_dir = "PATH_TO_OUTPUT_FOLDER")
writeLines(ft_attribution(ph), "attribution.txt")
```

## Quick start (runs as-is, no Python needed)

```r
library(FlowerTrait)
ex <- ft_example_photos()                       # CC0 / CC BY photos
ft_plot_outline(ex$crop[1], ex$mask[1], main = ex$species[1])
ft_traits(ex$crop, ex$mask)                     # core traits and outline QC in R
ft_symmetry(ex$mask)                            # symmetry and petal number
ft_plot_symmetry(ex$crop[2], ex$mask[2])        # radius profile and petal harmonic
res <- ft_example_results()                     # back-end results, default rule
summary(res)
ft_contact_sheet(res, ncol = 4)
ft_trait_info(verdict = "not reliable")
```

## Colour names and reflectance spectra

```r
ft_colour_name(ft_rgb_to_lab(rbind(c(220, 40, 30), c(120, 120, 220))))
ft_spectra_to_lab(wavelength, reflectance)      # spectra -> the colour space of the traits
ft_reference                                    # photo colour vs spectra for the validation species
```

## Decision rules

`ft_filter()` applies one of the rules compared on the benchmark photos
(`ft_benchmark$decision_rules`): `"qc"` (outline quality), `"default"`
(outline quality and photo filter), `"strict"` (default plus outline
stability) and `"face_view"` (strict plus a face-on view, for symmetry and
petal number). On the 487 benchmark photos, outline QC alone kept 93% of photos
with 5.9% wrong outlines; the default rule kept 75% with 1.1% wrong outlines.

## Functions

| Task | Functions |
|---|---|
| Photos | `ft_read_folder()`, `ft_taxonomy()`, `ft_gbif_photos()`, `ft_inat_photos()`, `ft_download_photos()`, `ft_attribution()`, `ft_example_photos()` |
| Back end | `ft_install()`, `ft_use_backend()`, `ft_backend_available()`, `ft_process()`, `ft_read_results()`, `ft_extended_traits()`, `ft_example_results()` |
| Traits in R | `ft_read_image()`, `ft_read_mask()`, `ft_write_mask()`, `ft_rgb_to_lab()`, `ft_traits()`, `ft_symmetry()`, `ft_colour_name()`, `ft_spectra_to_lab()` |
| Colour and augmentation | `ft_colour_check()`, `ft_white_balance()`, `ft_augment()`, `ft_augment_check()` |
| Decisions | `ft_filter()`, `summary()` and `plot()` methods |
| Reliability | `ft_trait_info()`, `ft_reliable_traits()`, `ft_icc()`, `ft_repeatability()`, `ft_invariance()` |
| Summaries and plots | `ft_summarise()`, `ft_plot_outline()`, `ft_plot_symmetry()`, `ft_plot_colours()`, `ft_contact_sheet()`, `ft_plot_traits()`, `ft_plot_repeatability()` |
| Data | `ft_catalogue`, `ft_validation`, `ft_benchmark`, `ft_reference` |

See `vignette("FlowerTrait")` for a worked example.

## Limitations

- Colour is device colour in sRGB under unknown light: no ultraviolet and no
  pollinator vision. Photographs preserve differences between species seen in
  spectra (`ft_reference`), but petals look lighter in photographs than in
  spectra; use spectra (e.g. with pavo) for visual modelling.
- The photo filter rejects few closed buds and fruits, which were rare in the
  training photos; keep the default restriction to observations annotated as
  flowering, or check the contact sheet.
- Symmetry traits describe how regular the outline is; they did not separate
  zygomorphic from radially symmetric species. `petal_n` matched visual counts
  for about 60% of face views (use with care).
- Absolute flower size needs a scale in the photo; `D_px` is in pixels.
- Petal number and symmetry are meaningful for face views only.
- The visual labels and outline ratings used to train and evaluate the photo
  filter were assigned with an AI assistant and were not compared with
  independent human labels (see the package article); the open-flower filter
  was compared with iNaturalist annotations.
- Repeatability comes from two photos of the same observation; variation across
  days, observers and cameras will be larger.
- SAM 2.1 is loaded through ultralytics, which is licensed under AGPL-3.0.

## License

GPL (>= 3). The example photos keep their own licences (CC0 1.0 or CC BY 4.0);
see `ft_example_photos()` and `inst/extdata/ATTRIBUTION.csv`.
