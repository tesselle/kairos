#' @details
#' \tabular{ll}{
#'  **Package:** \tab kairos \cr
#'  **Type:** \tab Package \cr
#'  **Version:** \tab 2.2.1 \cr
#'  **License:** \tab GPL-3 \cr
#'  **Zenodo:** \tab \doi{10.5281/zenodo.5653896} \cr
#' }
#'
#' @section Package options:
#'  \pkg{kairos} uses the following [options()] to configure behavior:
#'  * `kairos.progress`: a [`logical`] scalar. Should progress bars be
#'    displayed? Defaults to [interactive()].
#'  * `kairos.verbose`: a [`logical`] scalar. Should \R report extra information
#'    on progress? Defaults to [interactive()].
#'
#' @author
#' **Full list of authors and contributors** (alphabetic order)
#'
#' \tabular{ll}{
#'  Nicolas Frerebeau \tab *Université Bordeaux Montaigne, France* \cr
#'  Brice Lebrun \tab  \cr
#'  Ben Marwick \tab *University of Washington, USA* \cr
#'  Anne Philippe \tab *Université de Nantes, France* \cr
#' }
#'
#' **Package maintainer**
#'
#' Nicolas Frerebeau\cr
#' \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#' Archéosciences Bordeaux (UMR 6034)\cr
#' Maison de l'Archéologie\cr
#' Université Bordeaux Montaigne\cr
#' F-33607 Pessac cedex\cr
#' France
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
