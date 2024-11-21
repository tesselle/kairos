#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname permute
#' @aliases permute,data.frame,PermutationOrder-method
setMethod(
  f = "permute",
  signature = c(object = "data.frame", order = "PermutationOrder"),
  definition = function(object, order) {
    # Rearrange data.frame
    object[order@rows_order, order@columns_order]
  }
)

#' @export
#' @rdname permute
#' @aliases permute,matrix,PermutationOrder-method
setMethod(
  f = "permute",
  signature = c(object = "matrix", order = "PermutationOrder"),
  definition = function(object, order) {
    # Rearrange matrix
    object[order@rows_order, order@columns_order]
  }
)

#' @export
#' @rdname order
#' @aliases order_rows,PermutationOrder-method
setMethod(
  f = "order_rows",
  signature = c("PermutationOrder"),
  definition = function(object) object@rows_order
)

#' @export
#' @rdname order
#' @aliases order_columns,PermutationOrder-method
setMethod(
  f = "order_columns",
  signature = c("PermutationOrder"),
  definition = function(object) object@columns_order
)
