#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriation
#' @aliases permute,data.frame,PermutationOrder-method
setMethod(
  f = "permute",
  signature = signature(object = "data.frame", order = "PermutationOrder"),
  definition = function(object, order) {
    # Rearrange data.frame
    object[order[["rows"]], order[["columns"]]]
  }
)

#' @export
#' @rdname seriation
#' @aliases permute,matrix,PermutationOrder-method
setMethod(
  f = "permute",
  signature = signature(object = "matrix", order = "PermutationOrder"),
  definition = function(object, order) {
    # Rearrange matrix
    object[order[["rows"]], order[["columns"]]]
  }
)
