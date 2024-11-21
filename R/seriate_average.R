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
    ## Validation
    arkhe::assert_length(axes, 1)

    margin <- as.integer(margin)
    axes <- as.integer(axes)

    ## Original sequences
    i <- seq_len(nrow(object))
    j <- seq_len(ncol(object))

    ## Correspondence analysis
    corresp <- dimensio::ca(object, sup_row = sup_row, sup_col = sup_col)
    coords <- list(
      rows = dimensio::get_coordinates(corresp, margin = 1),
      columns = dimensio::get_coordinates(corresp, margin = 2)
    )

    ## Reorder in case if supplementary observations
    coords$rows <- coords$rows[order(corresp@rows@order), ]
    coords$columns <- coords$columns[order(corresp@columns@order), ]

    ## Seriation order
    row_coords <- if (any(margin == 1)) order(coords$rows[, axes]) else i
    col_coords <- if (any(margin == 2)) order(coords$columns[, axes]) else j

    ## New PermutationOrder object
    .AveragePermutationOrder(
      corresp,
      rows_order = as.integer(row_coords),
      columns_order = as.integer(col_coords)
    )
  }
)
