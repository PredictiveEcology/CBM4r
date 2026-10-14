# CBM4r

## Description

`CBM4r` is an R package interface to [CBM4](https://github.com/cat-cfs/cbm4) Python applications working with [CBM4 spatial datasets](https://github.com/cat-cfs/tech_docs/tree/main/dev/cbm4/structure#cbm4-spatial-datasets).

## Getting started

### Install R package from Github

``` r
remotes::install_github("PredictiveEcology/CBM4r", build_vignettes = TRUE)
```

### View package vignettes

If package was installed with vignettes:

``` r
browseVignettes("CBM4r")

```

Otherwise, view rendered vignettes:

- [Getting started](vignettes/Getting-started.md)
- [Example: Saskatchewan](vignettes/Example-SK.md)
- [Reading results with the SQLResultsProcessor](vignettes/Results.md)
