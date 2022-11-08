#' @rdname deprecate
#' @aliases refine-method
setGeneric(
  name = "refine",
  def = function(object, ...) standardGeneric("refine")
)

#' @export
#' @rdname deprecate
#' @aliases refine,CA-method
setMethod(
  f = "refine",
  signature = signature(object = "AveragePermutationOrder"),
  definition = function(object, cutoff, margin = c(1, 2), axes = c(1, 2), n = 30) {
    .Deprecated("seriate_refine()", old = "refine()")
    seriate_refine(object, cutoff, margin = margin, axes = axes, n = n)
  }
)

#' @export
#' @rdname deprecate
#' @aliases refine,BootstrapCA-method
setMethod(
  f = "refine",
  signature = signature(object = "BootstrapCA"),
  definition = function(object, cutoff, margin = 1, axes = c(1, 2)) {
    .Deprecated("seriate_refine()", old = "refine()")
    seriate_refine(object, cutoff, margin = margin, axes = axes)
  }
)
