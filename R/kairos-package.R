#' @details
#' \tabular{ll}{
#'  **Package:** \tab kairos \cr
#'  **Type:** \tab Package \cr
#'  **Version:** \tab 0.0.0.9000 \cr
#'  **License:** \tab GPL-3 \cr
#' }
#'
#' @section Package options:
#'  `kairos` uses the following [options()] to configure behavior:
#'  * `kairos.progress`: a [`logical`] scalar. Should progress bars be
#'    displayed?
#'
#' @author
#' **Full list of authors and contributors** (alphabetic order)
#'
#' \tabular{ll}{
#'  Nicolas Frerebeau \tab *Université Bordeaux Montaigne, France* \cr
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
#' @importFrom dimensio ca get_coordinates get_eigenvalues get_replications
#' predict
#' @importFrom extraDistr ptnorm
#' @importFrom ggplot2 autoplot ggplot aes facet_wrap geom_area geom_line
#' geom_point scale_x_continuous scale_y_continuous scale_y_discrete
#' theme theme_bw vars
#' @importFrom methods as callGeneric callNextMethod new setClass setGeneric
#' setMethod validObject .valueClassTest
#' @importFrom rlang .data
NULL

# /!\ Import conflictuel /!\
# La méthode générique de bootstrap() est définie dans arkhe ET dans dimensio.
# bootstrap() n'est pas formellement importé depuis dimensio (import depuis
# arkhe) : utiliser dimensio::bootstrap() au besoin.
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-namespaces
