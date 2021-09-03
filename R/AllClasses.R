# CLASSES DEFINITION AND INITIALIZATION
NULL

# DateMCD ======================================================================
#' Mean Ceramic Date
#'
#' An S4 class to store the Mean Ceramic Date of archaeological assemblages.
#' @slot dates_types A [`numeric`] vector giving the dates of the ceramic types.
#' @slot dates_mcd A [`numeric`] vector giving the mean ceramic dates.
#' @section Coerce:
#'  In the code snippets below, `x` is a `DateMCD` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases DateMCD-class
.DateMCD <- setClass(
  Class = "DateMCD",
  slots = c(
    dates_types = "numeric",
    dates_mcd = "numeric"
  ),
  contains = "CountMatrix"
)

# DateEvent ====================================================================
#' Date Model
#'
#' S4 classes to store the event and accumulation times of archaeological
#' assemblages.
#' @slot dates A [`numeric`] vector of dates.
#' @slot model A [multiple linear model][stats::lm()]: the Gaussian
#'  multiple linear regression model fitted for event date estimation and
#'  prediction.
#' @slot cutoff An length-one [`integer`] vector giving the cutoff value.
#' @slot keep An [`integer`] vector.
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
#' @slot p A [`numeric`] [`array`] giving the aorisitic probabilities.
#' @note This class inherits from base [`matrix`].
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
    p = "array"
  ),
  contains = "matrix"
)

#' Rate of Change
#'
#' An S4 class to represent rates of change from an aoristic analysis.
#' @slot replicates A non-negative [`integer`] giving the number of
#'  replications.
#' @slot blocks A [`character`] vector giving the time-blocks.
#' @slot groups A [`character`] vector.
#' @note This class inherits from base [`array`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases RateOfChange-class
.RateOfChange <- setClass(
  Class = "RateOfChange",
  slots = c(
    replicates = "integer",
    blocks = "character",
    groups = "character"
  ),
  contains = "array"
)

# CountApportion ==================================================================
#' Count Apportioning
#'
#' An S4 class to represent an artifact apportioning results. Gives the
#' apportioning of artifact types (columns) per site (rows) and per period
#' (dim. 3).
#' @slot p An [`array`] giving the probability of apportioning an artifact type
#'  to a given period.
#' @slot method A [`character`] string specifying the distribution used for
#'  apportioning (type popularity curve).
#' @slot from A length-one [`numeric`] vector giving the beginning of the
#'  period of interest (in years AD).
#' @slot to A length-one [`numeric`] vector giving the end of the period of
#'  interest (in years AD).
#' @slot step A length-one [`integer`] vector giving the step size, i.e. the
#'  width of each time step for apportioning (in years AD).
#' @note This class inherits from base [`array`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CountApportion-class
.CountApportion <- setClass(
  Class = "CountApportion",
  slots = c(
    p = "array",
    method = "character",
    from = "numeric",
    to = "numeric",
    step = "numeric"
  ),
  contains = "array"
)

# IncrementTest ================================================================
#' Frequency Increment Test
#'
#' An S4 class to represent a Frequency Increment Test results.
#' @slot counts A \eqn{m \times p}{m x p} [`numeric`] matrix of count data.
#' @slot dates A length-\eqn{m} [`numeric`] vector of dates.
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
    counts = "matrix",
    dates = "numeric",
    statistic = "numeric",
    parameter = "integer",
    p_value = "numeric"
  )
)
