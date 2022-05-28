#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriation
#' @aliases seriate_average,data.frame-method
setMethod(
  f = "seriate_average",
  signature = signature(object = "data.frame"),
  definition = function(object, margin = c(1, 2), axes = 1, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, margin = margin, axes = axes, ...)
  }
)

#' @export
#' @rdname seriation
#' @aliases seriate_average,matrix-method
setMethod(
  f = "seriate_average",
  signature = signature(object = "matrix"),
  definition = function(object, margin = c(1, 2), axes = 1, ...) {
    # Validation
    margin <- as.integer(margin)
    axes <- as.integer(axes)[[1L]]

    # Original sequences
    i <- seq_len(nrow(object))
    j <- seq_len(ncol(object))
    # Correspondence analysis
    corresp <- dimensio::ca(object, ...)
    # Sequence of the first axis as best seriation order
    coords <- list(
      rows = dimensio::get_coordinates(corresp, margin = 1),
      columns = dimensio::get_coordinates(corresp, margin = 2)
    )
    row_coords <- if (1 %in% margin) order(coords$rows[, axes]) else i
    col_coords <- if (2 %in% margin) order(coords$columns[, axes]) else j

    # New PermutationOrder object
    .AveragePermutationOrder(
      corresp,
      rows_order = as.integer(row_coords),
      columns_order = as.integer(col_coords)
    )
  }
)
