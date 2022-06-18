
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kairos <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/kairos/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/kairos/actions)
[![codecov](https://codecov.io/gh/tesselle/kairos/branch/master/graph/badge.svg)](https://app.codecov.io/gh/tesselle/kairos)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/kairos/badge)](https://www.codefactor.io/repository/github/tesselle/kairos)

<a href="https://tesselle.r-universe.dev" class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/kairos"
alt="r-universe" /></a>
<a href="https://cran.r-project.org/package=kairos"
class="pkgdown-release"><img
src="http://www.r-pkg.org/badges/version/kairos"
alt="CRAN Version" /></a> <a
href="https://cran.r-project.org/web/checks/check_results_kairos.html"
class="pkgdown-release"><img
src="https://cranchecks.info/badges/worst/kairos"
alt="CRAN checks" /></a>
<a href="https://cran.r-project.org/package=kairos"
class="pkgdown-release"><img
src="http://cranlogs.r-pkg.org/badges/kairos"
alt="CRAN Downloads" /></a>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5653896.svg)](https://doi.org/10.5281/zenodo.5653896)
<!-- badges: end -->

## Overview

A convenient and reproducible toolkit for relative and absolute dating
and analysis of chronological patterns. This package includes functions
for chronological modeling and dating of archaeological assemblages from
count data. It provides methods for matrix seriation. It also allows to
compute time point estimates and density estimates of the occupation and
duration of an archaeological site. **kairos** provides methods for:

-   Matrix seriation: `seriate_rank()` and `seriate_average()`
-   Mean ceramic date estimation (South 1977): `mcd()`
-   Event and accumulation date estimation (Bellanger and Husi 2012):
    `event()`
-   Aoristic analysis (Ratcliffe 2000): `aoristic()`
-   Chronological apportioning (Roberts et al. 2012): `apportion()`

[**tabula**](https://packages.tesselle.org/tabula/) is a companion
package to **kairos** that provides functions for visualization and
analysis of archaeological count data.

## Installation

You can install the released version of **kairos** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("kairos")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tesselle/kairos")
```

## Usage

``` r
## Load packages
library(tabula)
library(kairos)
```

**kairos** only supports dates expressed in CE years (BCE years must be
given as negative numbers). **All results are rounded to zero decimal
places** (sub-annual precision does not make sense in most situations).
You can change this behavior with `options(kairos.precision = x)` (for
`x` decimal places).

*It assumes that you keep your data tidy*: each variable (type/taxa)
must be saved in its own column and each observation (sample/case) must
be saved in its own row.

``` r
## Build an incidence matrix with random data
set.seed(12345)
incidence1 <- matrix(sample(0:1, 400, TRUE, c(0.6, 0.4)), nrow = 20)
incidence1 <- incidence1 > 0 # logical

## Get seriation order on rows and columns
## If no convergence is reached before the maximum number of iterations (100), 
## it stops with a warning.
(indices <- seriate_rank(incidence1, margin = c(1, 2), stop = 100))
#> Plus d’une classe "PermutationOrder" est trouvée en cache : Utilisation de la première, depuis l’espace de noms 'tabula'
#> Aussi défini par 'kairos'
#> <RankPermutationOrder>
#> Permutation order for matrix seriation:
#> - Row order: 1 4 20 3 9 16 19 10 13 2 11 7 17 5 6 18 14 15 8 12...
#> - Column order: 1 16 9 4 8 14 3 20 13 2 6 18 7 17 5 11 19 12 15 10...

## Permute rows and columns
incidence2 <- permute(incidence1, indices)

## Plot matrix
tabula::plot_heatmap(incidence1) +
  ggplot2::scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "black"))
tabula::plot_heatmap(incidence2) +
  ggplot2::scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "black"))
```

<img src="man/figures/README-seriation-1.png" width="50%" /><img src="man/figures/README-seriation-2.png" width="50%" />

## Contributing

Please note that the **kairos** project is released with a [Contributor
Code of Conduct](https://www.tesselle.org/conduct.html). By contributing
to this project, you agree to abide by its terms.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-bellanger2012" class="csl-entry">

Bellanger, Lise, and Philippe Husi. 2012. “Statistical Tool for Dating
and Interpreting Archaeological Contexts Using Pottery.” *Journal of
Archaeological Science* 39 (4): 777–90.
<https://doi.org/10.1016/j.jas.2011.06.031>.

</div>

<div id="ref-ratcliffe2000" class="csl-entry">

Ratcliffe, Jerry H. 2000. “Aoristic Analysis: The Spatial Interpretation
of Unspecific Temporal Events.” *International Journal of Geographical
Information Science* 14 (7): 669–79.
<https://doi.org/10.1080/136588100424963>.

</div>

<div id="ref-roberts2012" class="csl-entry">

Roberts, John M., Barbara J. Mills, Jeffery J. Clark, W. Randall Haas,
Deborah L. Huntley, and Meaghan A. Trowbridge. 2012. “A Method for
Chronological Apportioning of Ceramic Assemblages.” *Journal of
Archaeological Science* 39 (5): 1513–20.
<https://doi.org/10.1016/j.jas.2011.12.022>.

</div>

<div id="ref-south1977" class="csl-entry">

South, S. A. 1977. *Method and Theory in Historical Archaeology*.
Studies in Archeology. New York: Academic Press.

</div>

</div>
