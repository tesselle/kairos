#' @details
#' \tabular{ll}{
#'  **Package:** \tab kairos \cr
#'  **Type:** \tab Package \cr
#'  **Version:** \tab 2.1.0 \cr
#'  **License:** \tab GPL-3 \cr
#' }
#'
#' @section Package options:
#'  `kairos` uses the following [options()] to configure behavior:
#'  * `kairos.progress`: a [`logical`] scalar. Should progress bars be
#'    displayed?
#'  * `kairos.calendar`: a [`aion::TimeScale-class`] object (default calendar
#'    for printing).
#'
#' @author
#' **Full list of authors and contributors** (alphabetic order)
#'
#' \tabular{ll}{
#'  Nicolas Frerebeau \tab *Université Bordeaux Montaigne, France* \cr
#'  Brice Lebrun \tab *Université Bordeaux Montaigne, France* \cr
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
