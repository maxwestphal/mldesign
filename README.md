
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {mldesign} - an R package for meaningful data splitting in applied machine learning

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The {mldesign} package allows meaningful data splitting for supervised
machine learning tasks. The data splitting in {mldesign} is based on an
**estimand** definition which is in turn based on user-specified
constraints.

**Constraints** can be thought of generalized inclusion/exclusion
criteria for

1.  Test observations
2.  The relation between observations in train and test set
3.  The training dataset(s)

In contrast to traditional techniques (hold-out, cross-validation,
bootstrap), this leads to a deterministic data splitting.

## Installation

You can install the development version of mldesign from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("maxwestphal/mldesign")
```

## Getting started

An overview over important functions and classes and a minimal example
can be found in the “overview” vignette:

``` r
library(mldesign)
vignette("overview", "mldesign")
```

## Roadmap

Planned features include:

1.  Vast extension of traditional (random) data splitting methods
2.  Nested data splits
3.  Export of data splitting to popular ML packages
4.  print/summary/visualize functions

## Feedback
