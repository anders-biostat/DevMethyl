
# DevMethyl

<!-- badges: start -->
<!-- badges: end -->

The goal of DevMethyl is to analyse single-cell multi-omics data of cells at various differentiation states. 
It is used to visualize the changes of the epigenetic landscape (DNA methylation and chromatin accessibility) during stem cell progression, while providing access to genomic information like CpG content, genomic and regulatory features. 
This package was created to provide a easy access to summarized information on any specific genomic region of interest, enabling users to efficiently navigate through extensive data.

## Installation

You can install the development version of `DevMethyl` from [GitHub](https://github.com/).

``` r

install.packages("remotes")

remotes::install_github(
  "anders-biostat/DevMethyl",
  build_vignettes = TRUE,
  dependencies = TRUE)

```

## Vignette
For detailed examples and in-depth information, please refer to the package vignette.

You can view the vignette by running:
``` r
vignette("DevMethyl", package="DevMethyl")
```
