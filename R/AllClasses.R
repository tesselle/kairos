# CLASSES DEFINITION AND INITIALIZATION
NULL

# DateMCD ======================================================================
#' Mean Ceramic Date
#'
#' S4 classes to store the Mean Ceramic Date of archaeological assemblages.
#' @slot counts A [`numeric`] matrix of count data.
#' @slot dates A [`numeric`] vector of dates.
#' @slot mcd A [`numeric`] vector giving the mean ceramic dates.
#' @section Subset:
#'  In the code snippets below, `x` is a `DateMCD` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extracts information from a slot selected by subscript `i`.
#'   `i` is a length-one [`character`] vector. Returns the corresponding slot
#'   values.}
#'  }
#' @section Coerce:
#'  In the code snippets below, `x` is a `DateMCD` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @author N. Frerebeau
#' @seealso [`date_mcd()`]
#' @family classes
#' @docType class
#' @aliases DateMCD-class
.DateMCD <- setClass(
  Class = "DateMCD",
  slots = c(
    counts = "matrix",
    dates = "numeric",
    mcd = "numeric"
  )
)

# DateEvent ====================================================================
#' Date Model
#'
#' S4 classes to store the event and accumulation times of archaeological
#'  assemblages.
#' @slot data A [`numeric`] matrix of count data.
#' @slot dates A [`numeric`] vector of dates.
#' @slot model A [multiple linear model][stats::lm()]: the Gaussian
#'  multiple linear regression model fitted for event date estimation and
#'  prediction.
#' @slot cutoff An [`integer`] vector giving the cutoff value.
#' @slot dimension An [`integer`] vector giving the CA dimensions
#'  kept.
#' @slot level A length-one [`numeric`] vector giving the
#'  confidence level.
#' @slot row_events A four columns [`numeric`] matrix giving the
#'  predicted event dates for each archaeological assemblage, with the following
#'  columns:
#'  \describe{
#'   \item{date}{The event date estimation.}
#'   \item{lower}{The lower boundary of the confidence interval.}
#'   \item{upper}{The upper boundary of the confidence interval.}
#'   \item{error}{The standard error of predicted dates.}
#'  }
#' @slot column_events A four columns [`numeric`] matrix giving the
#'  predicted event dates for each archaeological type or fabric, with the
#'  following columns:
#'  \describe{
#'   \item{date}{The event date estimation.}
#'   \item{lower}{The lower boundary of the confidence interval.}
#'   \item{upper}{The upper boundary of the confidence interval.}
#'   \item{error}{The standard error of predicted dates.}
#'  }
#' @slot accumulation A two columns [`numeric`] matrix giving the
#'  point estimate of the accumulation dates and the corresponding error.
#' @section Subset:
#'  In the code snippets below, `x` is a `DateEvent` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extracts information from a slot selected by subscript `i`.
#'   `i` is a length-one [`character`] vector. Returns the corresponding slot
#'   values.}
#'  }
#' @seealso [`dimensio::CA-class`]
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases DateEvent-class
.DateEvent <- setClass(
  Class = "DateEvent",
  slots = c(
    dates = "numeric",
    model = "lm",
    cutoff = "integer",
    keep = "integer"
  ),
  contains = "CA"
)

# AoristicSum ==================================================================
#' Aoristic Sum
#'
#' An S4 class to represent an aoristic analysis results.
#' @slot from A [`numeric`] vector.
#' @slot to A [`numeric`] vector.
#' @slot weights A [`numeric`] vector.
#' @slot groups A [`character`] vector.
#' @slot dates A [`numeric`] vector.
#' @slot sum A [`numeric`] vector.
#' @section Subset:
#'  In the code snippets below, `x` is an `AoristicSum` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extracts information from a slot selected by subscript `i`.
#'   `i` is a length-one [`character`] vector. Returns the corresponding slot
#'   values.}
#'  }
#' @section Coerce:
#'  In the code snippets below, `x` is an `AoristicSum` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases AoristicSum-class
.AoristicSum <- setClass(
  Class = "AoristicSum",
  slots = c(
    from = "numeric",
    to = "numeric",
    weights = "numeric",
    groups = "character",
    dates = "numeric",
    sum = "numeric"
  )
)

# AoristicSum ==================================================================
#' Aoristic Sum
#'
#' An S4 class to represent an artifact apportioning results.
#' @slot p An [`array`] giving the probability of apportioning an artifact type
#'  to a given period.
#' @slot a An [`array`] giving the apportioning of artifact types per site and
#'  per period.
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CountApportion-class
.CountApportion <- setClass(
  Class = "CountApportion",
  slots = c(
    p = "array",
    a = "array"
  )
)

# IncrementTest ================================================================
#' Frequency Increment Test
#'
#' An S4 class to represent a Frequency Increment Test results.
#' @slot data A [`numeric`] matrix of count data.
#' @slot dates A [`numeric`] vector of dates.
#' @slot statistic A [`numeric`] vector giving the values of the t-statistic.
#' @slot parameter An [`integer`] giving the degrees of freedom for the
#'  t-statistic.
#' @slot p_value A [`numeric`] vector giving the the p-value for the test.
#' @section Subset:
#'  In the code snippets below, `x` is an `IncrementTest` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extracts information from a slot selected by subscript `i`.
#'   `i` is a length-one [`character`] vector. Returns the corresponding slot
#'   values.}
#'  }
#' @section Coerce:
#'  In the code snippets below, `x` is an `IncrementTest` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases IncrementTest-class
.IncrementTest <- setClass(
  Class = "IncrementTest",
  slots = c(
    data = "matrix",
    dates = "numeric",
    statistic = "numeric",
    parameter = "integer",
    p_value = "numeric"
  )
)
