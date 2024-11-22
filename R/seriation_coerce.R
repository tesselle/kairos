#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname as_seriation
#' @aliases as_seriation,CA-method
setMethod(
  f = "as_seriation",
  signature = c(object = "CA"),
  definition = function(object, margin = c(1, 2), axes = 1) {
    ## Validation
    arkhe::assert_length(axes, 1)

    margin <- as.integer(margin)
    axes <- as.integer(axes)

    ## Original sequences
    data <- dimensio::get_data(object)
    i <- seq_len(nrow(data))
    j <- seq_len(ncol(data))

    ## Correspondence analysis
    rows <- dimensio::get_coordinates(object, margin = 1)
    cols <- dimensio::get_coordinates(object, margin = 2)

    ## Reorder in case if supplementary observations
    rows <- rows[order(object@rows@order), ]
    cols <- cols[order(object@columns@order), ]

    ## Seriation order
    row_coords <- if (any(margin == 1)) order(rows[, axes]) else i
    col_coords <- if (any(margin == 2)) order(cols[, axes]) else j

    ## New PermutationOrder object
    .AveragePermutationOrder(
      object,
      rows_order = as.integer(row_coords),
      columns_order = as.integer(col_coords)
    )
  }
)
