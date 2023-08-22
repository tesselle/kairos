# SUMMARY
#' @include AllGenerics.R
NULL

#' @export
#' @method summary EventDate
summary.EventDate <- function(object, ...) {
  summary(object@model, ...)
}

#' @export
#' @rdname event
#' @aliases summary,EventDate,missing-method
setMethod("summary", c(object = "EventDate"), summary.EventDate)
