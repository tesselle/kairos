#' @details
#' \tabular{ll}{
#'  **Version** \tab 2.3.0 \cr
#'  **License** \tab GPL-3 \cr
#'  **CRAN DOI** \tab \doi{10.32614/CRAN.package.kairos} \cr
#'  **Zenodo DOI** \tab \doi{10.5281/zenodo.5653896} \cr
#' }
#'
#' Archéosciences Bordeaux (UMR 6034)\cr
#' Maison de l'Archéologie\cr
#' Université Bordeaux Montaigne\cr
#' F-33607 Pessac cedex\cr
#' France
#'
#' @section Package options:
#'  \pkg{kairos} uses the following [options()] to configure behavior:
#'  * `kairos.progress`: a [`logical`] scalar. Should progress bars be
#'    displayed? Defaults to [interactive()].
#'  * `kairos.verbose`: a [`logical`] scalar. Should \R report extra information
#'    on progress? Defaults to [interactive()].
#'
#' @name kairos-package
#' @aliases kairos
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @import arkhe
#' @import aion
#' @importFrom extraDistr ptnorm
#' @importFrom grDevices xy.coords
#' @importFrom methods as callGeneric callNextMethod new setClass setGeneric
#' setMethod validObject .valueClassTest
NULL
