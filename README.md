
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {mldesign} - An R package for structured data splitting to facilitate estimand-aligned performance estimation in machine learning

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/devel%20version-0.2.0-blue.svg)](https://github.com/maxwestphal/mldesign)
[![](https://www.r-pkg.org/badges/version/mldesign?color=orange)](https://cran.r-project.org/package=mldesign)
[![R-CMD-check](https://github.com/maxwestphal/mldesign/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/maxwestphal/mldesign/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/maxwestphal/mldesign/branch/main/graph/badge.svg)](https://app.codecov.io/gh/maxwestphal/mldesign?branch=main)
<!-- badges: end -->

The {mldesign} package allows structured data splitting for supervised
machine learning tasks. The data splitting in {mldesign} is based on an
**estimand** definition which is in turn based on user-specified
constraints.

**Constraints** can be thought of as generalized inclusion/exclusion
criteria for

1.  Test observations
2.  The relation between observations in training and test sets
3.  The training dataset(s)

In contrast to traditional techniques (hold-out, cross-validation,
bootstrap), this leads to a deterministic data splitting. The intention
behind structured data splitting is to allow estimation of
**estimand-aligned transferabiliy (out-of-distribution generalization)**
instead of **reproducibility (in distribution generalization)** (Alpers
& Westphal, 2025).

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

Planned features for future versions include:

1.  Extension of traditional (unstructured) data splitting methods
2.  Export of data splitting schemes to popular ML packages
3.  Improved summary/visualization functions for derived splits

## References

- Alpers, R. and Westphal, M. 2025. An estimand framework to guide model
  and algorithm validation in predictive modelling. Submitted for
  publication.
