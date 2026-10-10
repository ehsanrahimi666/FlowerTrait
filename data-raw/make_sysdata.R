# CIE 1931 2-degree colour-matching functions and the D65 illuminant at 1 nm, 360-830 nm
# (CIE 15:2018 tables as distributed with the 'colour-science' Python package; see cie1931_d65.csv).
.ft_cie <- utils::read.csv("data-raw/cie1931_d65.csv")
usethis_ok <- requireNamespace("usethis", quietly = TRUE)
save(.ft_cie, file = "R/sysdata.rda", compress = "xz")
