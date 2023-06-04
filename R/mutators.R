# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @rdname mutators
#' @aliases get_dates,EventDate-method
setMethod("get_dates", "EventDate", function(x) x@dates)

#' @export
#' @rdname mutators
#' @aliases get_model,EventDate-method
setMethod("get_model", "EventDate", function(x) x@model)

#' @export
#' @rdname mutators
#' @aliases get_weights,CountApportion-method
setMethod("get_weights", "CountApportion", function(x) x@p)

# Setters ======================================================================
