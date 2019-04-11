## Introduction

A package to `permute` and `bootstrap` an analysis using `lm` and append useful statistics to a `tidy` table

## Functions

1. **tidy_lm_bootstrap_permute**: `permute` and `bootstrap` `lm` and output the results as a `tidy` table.
2. **tidy_lm_permute**: `permute` `lm` and output the results as a `tidy` table.
2. **tidy_lm_add_r_squared**: add r.squared and adj.r.squared to `tidy` table of `lm` results.
3. **tidy_lm_add_logical_significance**: add a logical column of significance comparing *p*-values to *alpha*

## Installation

```{r}
devtools::install_github("epongpipat/bootPermBroom")
```

## Acknowledgements

This package relies on the following `tidyverse` packages: `broom`, `dplyr`, `modelr`, and `tibble`.
