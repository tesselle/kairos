# PLOT LINE
#' @include AllClasses.R AllGenerics.R
NULL

# data.frame ===================================================================
#' @export
#' @rdname plot_time
#' @aliases plot_time,data.frame,numeric-method
setMethod(
  f = "plot_time",
  signature = c(object = "data.frame", dates = "numeric"),
  definition = function(object, dates,
                        calendar = getOption("kairos.calendar"), ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, dates = dates, calendar = calendar, ...)
    invisible(object)
  }
)

# matrix =======================================================================
#' @export
#' @rdname plot_time
#' @aliases plot_time,matrix,numeric-method
setMethod(
  f = "plot_time",
  signature = c(object = "matrix", dates = "numeric"),
  definition = function(object, dates,
                        calendar = getOption("kairos.calendar"), ...) {
    ## Validation
    arkhe::assert_type(dates, "numeric")
    arkhe::assert_length(dates, nrow(object))

    ## Convert to rata die
    if (is.null(calendar)) {
      dates <- aion::as_fixed(dates)
    } else {
      dates <- aion::fixed(dates, calendar = calendar)
    }

    ## Prepare data
    ts <- aion::series(object = object, time = dates)

    ## Plot
    aion::plot(x = ts, calendar = calendar, ...)

    invisible(object)
  }
)
