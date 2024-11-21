#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriate_rank
#' @aliases seriate_rank,data.frame-method
setMethod(
  f = "seriate_rank",
  signature = c(object = "data.frame"),
  definition = function(object, EPPM = FALSE, margin = c(1, 2), stop = 100) {
    object <- data.matrix(object)
    methods::callGeneric(object, EPPM = EPPM, margin = margin, stop = stop)
  }
)

#' @export
#' @rdname seriate_rank
#' @aliases seriate_rank,matrix-method
setMethod(
  f = "seriate_rank",
  signature = c(object = "matrix"),
  definition = function(object, EPPM = FALSE, margin = c(1, 2), stop = 100) {
    ## Validation
    margin <- as.integer(margin)
    stop <- as.integer(stop)

    data <- object
    if (EPPM) data <- tabula::eppm(object)

    ## Compute ranks
    ## margin = 1 : on rows
    ## margin = 2 : on columns
    reorder <- function(x, margin) {
      i <- seq_len(nrow(x))
      j <- seq_len(ncol(x))
      if (margin == 1) k <- colSums(t(x) * j) / rowSums(x)
      if (margin == 2) k <- colSums(x * i) / colSums(x)
      order(k)
    }

    start <- 0
    index <- list(rows = seq_len(nrow(data)), columns = seq_len(ncol(data)))
    convergence <- FALSE
    while (!convergence) {
      old_index <- index
      # Rearrange along margins
      for (k in margin) {
        index[[k]] <- index[[k]][reorder(data[index[[1]], index[[2]]], margin = k)]
      }
      # Loop counter
      convergence <- identical(index, old_index)
      start <- start + 1
      if (start >= stop) {
        msg <- tr_("Convergence not reached (possible infinite loop).")
        warning(msg, call. = FALSE)
        break
      }
    }

    # New PermutationOrder object
    .RankPermutationOrder(
      rows_order = as.integer(index[[1]]),
      columns_order = as.integer(index[[2]])
    )
  }
)
