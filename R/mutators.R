# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @rdname mutators
#' @aliases get_dates,AoristicSum-method
setMethod("get_dates", "AoristicSum", function(x) x@dates)

#' @export
#' @rdname mutators
#' @aliases get_dates,RateOfChange-method
setMethod("get_dates", "RateOfChange", function(x) x@blocks)

#' @export
#' @rdname mutators
#' @aliases get_groups,AoristicSum-method
setMethod("get_groups", "AoristicSum", function(x) x@groups)

#' @export
#' @rdname mutators
#' @aliases get_mcd,DateMCD-method
setMethod("get_mcd", "DateMCD", function(x) x@dates_mcd)

#' @export
#' @rdname mutators
#' @aliases get_weights,AoristicSum-method
setMethod("get_weights", "AoristicSum", function(x) x@p)

#' @export
#' @rdname mutators
#' @aliases get_weights,CountApportion-method
setMethod("get_weights", "CountApportion", function(x) x@p)

# Setters ======================================================================
