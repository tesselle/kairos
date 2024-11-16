# EVENT MODEL
#' @include AllGenerics.R
NULL

# TODO: use aion::calendar_year() once released.

#' @export
#' @method summary EventDate
summary.EventDate <- function(object, ...) {
  summary(object@model, ...)
}

#' @export
#' @rdname model_event
#' @aliases summary,EventDate,missing-method
setMethod("summary", c(object = "EventDate"), summary.EventDate)

#' @method coef EventDate
#' @export
coef.EventDate <- function(object, calendar = NULL, ...) {
  z <- stats::coef(object@model, ...)
  if (is.null(calendar)) return(z)
  unclass(z) / calendar@year # Approximate
}

#' @export
#' @rdname model_event
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
#' @rdname model_event
#' @aliases fitted,EventDate-method
setMethod("fitted", "EventDate", fitted.EventDate)

#' @method residuals EventDate
#' @export
residuals.EventDate <- function(object, calendar = NULL, ...) {
  z <- stats::residuals(object@model, ...)
  if (is.null(calendar)) return(z)
  unclass(z) / calendar@year # Approximate
}

#' @export
#' @rdname model_event
#' @aliases residuals,EventDate-method
setMethod("residuals", "EventDate", residuals.EventDate)

#' @method sigma EventDate
#' @export
sigma.EventDate <- function(object, calendar = NULL, ...) {
  z <- stats::sigma(object@model, ...)
  if (is.null(calendar)) return(z)
  unclass(z) / calendar@year # Approximate
}

#' @export
#' @rdname model_event
#' @aliases sigma,EventDate-method
setMethod("sigma", "EventDate", sigma.EventDate)

#' @method terms EventDate
#' @export
terms.EventDate <- function(x, ...) stats::terms(x@model, ...)

#' @export
#' @rdname model_event
#' @aliases terms,EventDate-method
setMethod("terms", "EventDate", terms.EventDate)
