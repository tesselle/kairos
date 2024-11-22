#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriate_average
#' @aliases seriate_average,data.frame-method
setMethod(
  f = "seriate_average",
  signature = c(object = "data.frame"),
  definition = function(object, margin = c(1, 2), axes = 1,
                        sup_row = NULL, sup_col = NULL, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, margin = margin, axes = axes,
                         sup_row = sup_row, sup_col = sup_col, ...)
  }
)

#' @export
#' @rdname seriate_average
#' @aliases seriate_average,matrix-method
setMethod(
  f = "seriate_average",
  signature = c(object = "matrix"),
  definition = function(object, margin = c(1, 2), axes = 1,
                        sup_row = NULL, sup_col = NULL, ...) {
    ## Correspondence analysis
    corresp <- dimensio::ca(object, sup_row = sup_row, sup_col = sup_col)

    ## New PermutationOrder object
    as_seriation(corresp, margin = margin, axes = axes)
  }
)
