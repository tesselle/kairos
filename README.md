
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kairos <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/kairos/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/kairos/actions)
[![codecov](https://codecov.io/gh/tesselle/kairos/branch/master/graph/badge.svg)](https://app.codecov.io/gh/tesselle/kairos)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/kairos/badge)](https://www.codefactor.io/repository/github/tesselle/kairos)

[![r-universe](https://tesselle.r-universe.dev/badges/kairos)](https://tesselle.r-universe.dev)

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

## Overview

A toolkit for absolute dating and analysis of chronological patterns.
This package includes functions for chronological modeling and dating of
archaeological assemblages from count data. It allows to compute time
point estimates and density estimates of the occupation and duration of
an archaeological site. **kairos** provides methods for:

-   Mean ceramic date estimation (South 1977): `mcd()`
-   Event and accumulation date estimation (Bellanger and Husi 2012):
    `event()`
-   Aoristic analysis (Ratcliffe 2000): `aoristic()`
-   Chronological apportioning (Roberts et al. 2012): `apportion()`

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
library(kairos)
library(folio) # datasets
```

**kairos** only supports dates expressed in CE years (BCE years must be
given as negative numbers). **All results are rounded to zero decimal
places** (sub-annual precision does not make sense in most situations).
You can change this behavior with `options(kairos.precision = x)` (for
`x` decimal places).

**kairos** uses a set of S4 classes that represent different special
types of matrix. Please refer to the documentation of the
[**arkhe**](https://github.com/tesselle/arkhe) package where these
classes are defined.

*It assumes that you keep your data tidy*: each variable (type/taxa)
must be saved in its own column and each observation (sample/case) must
be saved in its own row.

``` r
## Aoristic Analysis
data("zuni", package = "folio")

## Set the start and end dates for each ceramic type
dates <- list(
  LINO = c(600, 875), KIAT = c(850, 950), RED = c(900, 1050),
  GALL = c(1025, 1125), ESC = c(1050, 1150), PUBW = c(1050, 1150),
  RES = c(1000, 1200), TULA = c(1175, 1300), PINE = c(1275, 1350),
  PUBR = c(1000, 1200), WING = c(1100, 1200), WIPO = c(1125, 1225),
  SJ = c(1200, 1300), LSJ = c(1250, 1300), SPR = c(1250, 1300),
  PINER = c(1275, 1325), HESH = c(1275, 1450), KWAK = c(1275, 1450)
)

## Keep only assemblages that have a sample size of at least 10
keep <- apply(X = zuni, MARGIN = 1, FUN = function(x) sum(x) >= 10)

## Calculate date ranges for each assemblage
span <- apply(
  X = zuni[keep, ],
  FUN = function(x, dates) range(unlist(dates[x > 0])),
  MARGIN = 1,
  dates = dates
)

## Coerce to data.frame
span <- as.data.frame(t(span))
colnames(span) <- c("from", "to")

## Calculate aoristic sum (weights)
aorist_weigth <- aoristic(span, step = 50, weight = TRUE)
plot(aorist_weigth)
```

![](man/figures/README-aoristic-1.png)<!-- -->

``` r
## Rate of change
set.seed(12345)
aorist_roc <- roc(aorist_weigth, n = 30)
plot(aorist_roc)
```

![](man/figures/README-aoristic-2.png)<!-- -->

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
