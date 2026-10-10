# Quick start: runs as-is with the files shipped in the package (no Python).
library(FlowerTrait)

ex <- ft_example_photos()
ex[, c("photo_id", "species", "family", "region", "rights_holder", "license")]

# Core colour, radial-pattern, symmetry and shape traits and outline quality from the shipped crops and outlines
tr <- ft_traits(ex$crop, ex$mask)
tr[, c("photo_id", "mask_ok", "D_px", "L_mean", "C_mean", "colour_name", "centre_dL", "petal_n", "sym_rotational")]

# Symmetry and petal number of one outline
ft_symmetry(ex$mask[2])
ft_plot_symmetry(ex$crop[2], ex$mask[2])

# Back-end results with the default decision rule
res <- ft_example_results()
summary(res)
ft_contact_sheet(res, ncol = 4)

# Trait reliability
table(ft_catalogue$family, ft_catalogue$verdict)
ft_trait_info(verdict = c("use with care", "not reliable"))

# Repeatability on the validation photos (two photos per plant)
v <- ft_filter(ft_validation, "default")
ft_repeatability(v[v$keep, ], traits = c("L_mean", "C_mean", "petal_L", "centre_dL"), boot = 100)

# Photo colour against reflectance spectra of the same species
head(ft_reference[, c("species", "region", "ref_name", "photo_name", "dE00")])
