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
#' @rdname permute
#' @aliases get_order,PermutationOrder-method
setMethod(
  f = "get_order",
  signature = c("PermutationOrder"),
  definition = function(x, margin = c(1, 2)) {
    o <- list(rows = integer(0), columns = integer(0))
    o$rows <- if (1 %in% margin) x@rows_order else NULL
    o$columns <- if (2 %in% margin) x@columns_order else NULL
    if (length(o) == 1) unname(unlist(o)) else o
  }
)
