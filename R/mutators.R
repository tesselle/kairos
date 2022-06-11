# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @rdname mutators
#' @aliases get_dates,AoristicSum-method
setMethod("get_dates", "AoristicSum", function(x) {
  breaks <- x@breaks
  data.frame(
    start = utils::head(breaks, -1),
    end = utils::tail(breaks, -1)
  )
})

#' @export
#' @rdname mutators
#' @aliases get_dates,EventDate-method
setMethod("get_dates", "EventDate", function(x) x@dates)

#' @export
#' @rdname mutators
#' @aliases get_dates,RateOfChange-method
setMethod("get_dates", "RateOfChange", function(x) x@breaks)

#' @export
#' @rdname mutators
#' @aliases get_groups,AoristicSum-method
setMethod("get_groups", "AoristicSum", function(x) x@groups)

#' @export
#' @rdname mutators
#' @aliases get_model,EventDate-method
setMethod("get_model", "EventDate", function(x) x@model)

#' @export
#' @rdname mutators
#' @aliases get_weights,AoristicSum-method
setMethod("get_weights", "AoristicSum", function(x) x@p)

#' @export
#' @rdname mutators
#' @aliases get_weights,CountApportion-method
setMethod("get_weights", "CountApportion", function(x) x@p)

#' @export
#' @rdname mutators
#' @aliases get_weights,MeanDate-method
setMethod("get_weights", "MeanDate", function(x) x@weights)

# Setters ======================================================================
