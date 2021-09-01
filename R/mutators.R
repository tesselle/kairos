# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @rdname mutators
#' @aliases get_mcd,DateMCD-method
setMethod("get_mcd", "DateMCD", function(x) x@dates_mcd)

# Setters ======================================================================
