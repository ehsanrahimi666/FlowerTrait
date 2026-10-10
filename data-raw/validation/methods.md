# Validation of FlowerTrait: data and scripts

This folder holds the inputs of `data-raw/make_datasets.R`, which builds the package data sets
`ft_validation`, `ft_catalogue`, `ft_benchmark` and `ft_reference`, and the scripts that produced them
(`code/`). The scripts record what was run; their paths refer to the working folder of the validation and
must be adapted before they are run elsewhere.

## Photographs

- `photo_set.csv`: the validation photographs (research-grade iNaturalist observations retrieved through the
  GBIF occurrence API; GBIF download https://doi.org/10.15468/dl.24hc8u), with species, the name used by the
  reference spectra (`ref_species`), region, coordinates, the iNaturalist phenology annotation
  (`reproductiveCondition`), photo URL, licence (CC0 1.0 or CC BY 4.0) and rights holder.
  Roles: `benchmark` (first photo of seven flowering observations per species), `pair_sibling` (second photo of
  two benchmark observations per species), `pair_new` (two photos of one more flowering observation per
  species) and `negative` (one observation per species annotated as not flowering).
  Species selection: `code/select_species.py`; photo selection: `code/select_photos.py`.
- `traits_all.csv`: back-end output for every photograph (`flowertrait.run_photos --no-filter --embeddings`,
  SAM 2.1 base, two CPU cores); `code/merge_runs.py` merges the two parallel runs.
- `labels.csv`: visual labels of the benchmark photographs and outline ratings (codes in `?ft_validation`),
  assigned from contact sheets (`code/label_sheets.py`, `code/sheet.py`) that show the crop with the outline
  (background darkened) and the whole photograph; see the AI disclosure statement of the package article.
- `filter_eval.csv`, `filter_preds.csv`: cross-validated evaluation of the photo-filter heads and out-of-fold
  probabilities (`code/train_filter.py`); the heads shipped with the package are in
  `inst/python/flowertrait/data/filter_heads.json`.

## Reference data

- `reference_spectra.csv`: floral reflectance spectra (300–700 nm, 1 nm, percent) of the validation species
  from the Floral Reflectance Database (FReD; Arnold et al. 2010, https://doi.org/10.1371/journal.pone.0014287)
  and five Dryad data sets: Kent Island, Canada (https://doi.org/10.5061/dryad.9p8cz8wtm); serpentine seeps,
  California (https://doi.org/10.5061/dryad.v41ns1rtq); Namaqualand daisies
  (https://doi.org/10.5061/dryad.4pn6413); *Erica*, Western Cape (https://doi.org/10.5061/dryad.6djh9w1gz);
  Australian flowers (https://doi.org/10.5061/dryad.fn2z34v2c). Only the spectra of the validation species are
  included. Built with `code/build_reference.py`, which also read further data sets that contributed no
  validation species.
- `oxford_results.csv`: automatic outlines compared with the trimaps of the Oxford 17 Category Flower
  Dataset (Nilsback & Zisserman 2006; https://www.robots.ox.ac.uk/~vgg/data/flowers/17/), six photographs per
  class (`code/oxford_bench.py`; the class Cowslip has no trimaps). The photographs and trimaps are not
  redistributed.

## Analyses

- `invariance_long.csv`: traits of benchmark photographs that passed the strict rule after simulated camera
  changes and size reduction (`code/invariance.py`).
- `white_balance.csv`, `white_balance_reference.csv`, `augmentation.csv`, `annotations.csv`: colour
  correction, augmentation check and comparison with the iNaturalist annotations (`code/colour_analysis.R`).
- `colour_check.csv`: `ft_colour_check()` of all validation crops with a valid outline
  (`code/colour_check_all.R`).
- R and Python agreement: `code/py_core_traits.py` and `code/r_py_agree.R`.
- Example photographs and gallery thumbnails in `inst/extdata`: `code/make_examples.py`.
- `trait_descriptions.csv`: definitions of the traits (`ft_catalogue`).
