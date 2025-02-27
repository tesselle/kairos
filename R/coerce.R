# COERCE
#' @include AllGenerics.R
NULL

# To data.frame ================================================================
#' @export
#' @method as.data.frame MeanDate
as.data.frame.MeanDate <- function(x, ..., calendar = get_calendar()) {
  data.frame(
    sample = rownames(x) %||% paste0("S", seq_len(nrow(x))),
    time = aion::time(x, calendar = calendar)
  )
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,MeanDate-method
setMethod("as.data.frame", "MeanDate", as.data.frame.MeanDate)

#' @export
#' @method as.data.frame AoristicSum
as.data.frame.AoristicSum <- function(x, ..., calendar = get_calendar()) {
  block_start <- utils::head(x@breaks, -1)
  block_end <- utils::tail(x@breaks, -1)

  aorist <- x[, , 1, drop = TRUE]
  dim(aorist) <- c(nrow(x), ncol(x))
  colnames(aorist) <- colnames(x) %||% paste0("S", seq_len(ncol(x)))

  data.frame(
    start = aion::as_year(block_start, calendar = calendar),
    end = aion::as_year(block_end, calendar = calendar),
    aorist
  )
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,AoristicSum-method
setMethod("as.data.frame", "AoristicSum", as.data.frame.AoristicSum)

#' @method as.data.frame IncrementTest
#' @export
as.data.frame.IncrementTest <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(
    t = x@statistic,
    p.value = x@p_value,
    row.names = colnames(x),
    ...
  )
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,IncrementTest-method
setMethod("as.data.frame", "IncrementTest", as.data.frame.IncrementTest)
