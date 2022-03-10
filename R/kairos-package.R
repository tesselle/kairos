#' @details
#' \tabular{ll}{
#'  **Package:** \tab kairos \cr
#'  **Type:** \tab Package \cr
#'  **Version:** \tab 1.0.1 \cr
#'  **License:** \tab GPL-3 \cr
#' }
#'
#' @section Package options:
#'  `kairos` uses the following [options()] to configure behavior:
#'  * `kairos.precision`: an [`integer`] indicating the number of decimal
#'    places for years (defaults to \eqn{0}).
#'  * `kairos.progress`: a [`logical`] scalar. Should progress bars be
#'    displayed?
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
#' IRAMAT-CRP2A (UMR 5060)\cr
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
#' @importFrom dimensio bootstrap ca get_coordinates get_eigenvalues
#' get_replications jackknife predict
#' @importFrom extraDistr ptnorm
#' @importFrom ggplot2 autoplot ggplot aes facet_wrap geom_area geom_line
#' geom_point geom_segment mean_cl_normal scale_x_continuous scale_y_continuous
#' scale_y_discrete stat_summary theme theme_bw vars
#' @importFrom methods as callGeneric callNextMethod new setClass setGeneric
#' setMethod validObject .valueClassTest
#' @importFrom rlang .data
#' @importFrom stats weighted.mean
#' @importFrom utils head
NULL
