# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @rdname mutators
#' @aliases get_dates,DateMCD-method
setMethod("get_dates", "DateMCD", function(x) x@dates_mcd)

# Setters ======================================================================
