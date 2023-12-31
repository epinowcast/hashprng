
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Hash-Based Matching Pseudo-Random Number Generation <a href='https://hashprng.epinowcast.org'><img src='man/figures/logo.png' align="right" height="139" /></a>

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/epinowcast/hashprng/workflows/R-CMD-check/badge.svg)](https://github.com/epinowcast/hashprng/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epinowcast/hashprng/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epinowcast/hashprng)

[![Universe](https://epinowcast.r-universe.dev/badges/hashprng)](https://epinowcast.r-universe.dev/hashprng)
[![MIT
license](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/epinowcast/hashprng/blob/main/LICENSE.md/)
[![GitHub
contributors](https://img.shields.io/github/contributors/epinowcast/hashprng)](https://github.com/epinowcast/hashprng/graphs/contributors)

Helper functions for use of hash-based matching (HBM) for pseudo-random
number generation (PRNG) in stochastic simulations. HBM-PRNG is an
approach to simplify matching synthetic experiment samples, which
ensures that matched runs different only in the focal parameters, not in
their chance events.

## Getting started and learning more

This README is a good place to get started with `hashprng`, in
particular the following installation and quick start sections. As you
make use of the package, or if your problem requires a richer feature
set than presented here, we also provide a range of other reosources.

- [Package website](https://hashprng.epinowcast.org/): This includes a
  function reference, model outline, and case studies making use of the
  package. This site refers to the release version of our package. The
  development version of our documentation (corresponding to our `main`
  branch on GitHub) is available
  [here](https://hashprng.epinowcast.org/dev/).

## Installation

### Installing the package

Install the latest GitHub released version of the package with:

``` r
install.packages("hashprng", repos = "https://epinowcast.r-universe.dev")
```

Install the development version (whilst we strive to limit breaking
changes or the introduction of bugs during development this version may
contain both) from GitHub using the following,

``` r
remotes::install_github("epinowcast/hashprng", dependencies = TRUE)
```

## Quick start

In this quick start …

## Citation

If you use `hashprng` in your work, please consider citing it using the
following,

    #> To cite package 'hashprng' in publications use:
    #> 
    #>   Pearson C (2023). _hashprng: Hash-Based Matching Pseudo-Random Number
    #>   Generation_. R package version 0.3.0.1000.
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Manual{,
    #>     title = {hashprng: Hash-Based Matching Pseudo-Random Number Generation},
    #>     author = {Carl Pearson},
    #>     year = {2023},
    #>     note = {R package version 0.3.0.1000},
    #>   }

## How to make a bug report or feature request

Please briefly describe your problem and what output you expect in an
[issue](https://github.com/epinowcast/hashprng/issues). If you have a
question, please don’t open an issue. Instead, ask on our [Q and A
page](https://github.com/epinowcast/hashprng/discussions/categories/q-a).
See our [contributing
guide](https://github.com/epinowcast/hashprng/blob/main/CONTRIBUTING.md)
for more information.

## Contributing

We welcome contributions and new contributors! We particularly
appreciate help on priority problems in the
[issues](https://github.com/epinowcast/hashprng/issues). Please check
and add to the issues, and/or add a [pull
request](https://github.com/epinowcast/hashprng/pulls). See our
[contributing
guide](https://github.com/epinowcast/hashprng/blob/main/CONTRIBUTING.md)
for more information.

## Code of Conduct

Please note that the hashprng project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
