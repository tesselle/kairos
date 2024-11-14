#' Deprecated Functions in kairos
#'
#' These functions still work but will be removed (defunct) in the next version.
#' @name kairos-deprecated
#' @keywords internal
NULL

#' @export
#' @rdname kairos-deprecated
setGeneric(
  name = "get_order",
  def = function(x, ...) standardGeneric("get_order")
)

#' @export
#' @rdname kairos-deprecated
setMethod(
  f = "get_order",
  signature = c("PermutationOrder"),
  definition = function(x, margin = c(1, 2)) {
    .Deprecated(new = "order_rows()/order_columns()", old = "get_order()")
    o <- list(rows = integer(0), columns = integer(0))
    o$rows <- if (1 %in% margin) x@rows_order else NULL
    o$columns <- if (2 %in% margin) x@columns_order else NULL
    if (length(o) == 1) unname(unlist(o)) else o
  }
)
