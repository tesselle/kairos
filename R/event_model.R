# EVENT MODEL
#' @include AllGenerics.R
NULL

#' @method coef EventDate
#' @export
coef.EventDate <- function(object, calendar = NULL, ...) {
  z <- stats::coef(object@model, ...)
  if (is.null(calendar)) return(z)
  aion::as_year(z, calendar = calendar) - aion::calendar_fixed(calendar)
}

#' @export
#' @rdname model
#' @aliases coef,EventDate-method
setMethod("coef", "EventDate", coef.EventDate)

#' @method fitted EventDate
#' @export
fitted.EventDate <- function(object, calendar = NULL, ...) {
  z <- stats::fitted(object@model, ...)
  if (is.null(calendar)) return(z)
  aion::as_year(z, calendar = calendar)
}

#' @export
#' @rdname model
#' @aliases fitted,EventDate-method
setMethod("fitted", "EventDate", fitted.EventDate)

#' @method residuals EventDate
#' @export
residuals.EventDate <- function(object, calendar = NULL, ...) {
  z <- stats::residuals(object@model, ...)
  if (is.null(calendar)) return(z)
  aion::as_year(z, calendar = calendar) - aion::calendar_fixed(calendar)
}

#' @export
#' @rdname model
#' @aliases residuals,EventDate-method
setMethod("residuals", "EventDate", residuals.EventDate)

#' @method sigma EventDate
#' @export
sigma.EventDate <- function(object, calendar = NULL, ...) {
  z <- stats::sigma(object@model, ...)
  if (is.null(calendar)) return(z)
  aion::as_year(z, calendar = calendar) - aion::calendar_fixed(calendar)
}

#' @export
#' @rdname model
#' @aliases sigma,EventDate-method
setMethod("sigma", "EventDate", sigma.EventDate)

#' @method terms EventDate
#' @export
terms.EventDate <- function(x, ...) stats::terms(x@model, ...)

#' @export
#' @rdname model
#' @aliases terms,EventDate-method
setMethod("terms", "EventDate", terms.EventDate)
