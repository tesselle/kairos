# CLASSES DEFINITION AND INITIALIZATION
NULL

# MeanDate ======================================================================
#' Mean Date
#'
#' An S4 class to store the weighted mean date (e.g. Mean Ceramic Date) of
#' archaeological assemblages.
#' @slot dates A length-\eqn{p} [`numeric`] vector giving the dates of the
#'  (ceramic) types expressed in *[rata die][aion::RataDie-class]*.
#' @slot replications A `numeric` [`matrix`] giving the replications.
#' @section Coerce:
#'  In the code snippets below, `x` is a `MeanDate` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @note
#'  This class inherits from [`aion::TimeSeries-class`]: dates are internally
#'  stored as *[rata die][aion::RataDie-class]*.
#' @seealso [`aion::TimeSeries-class`]
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases MeanDate-class
#' @keywords internal
.MeanDate <- setClass(
  Class = "MeanDate",
  slots = c(
    dates = "RataDie"
  ),
  contains = "TimeSeries"
)

.SimulationMeanDate <- setClass(
  Class = "SimulationMeanDate",
  slots = c(
    replications = "matrix"
  ),
  contains = "MeanDate"
)

# EventDate ====================================================================
#' Date Model
#'
#' An S4 class to store the event and accumulation times of archaeological
#' assemblages.
#' @slot contexts A \eqn{m \times p}{m x p} `integer` [`matrix`] of count
#'  data.
#' @slot dates A length-\eqn{m} [`numeric`] vector of dates expressed in
#'  *[rata die][aion::RataDie-class]*.
#' @slot model A [multiple linear model][stats::lm()]: the Gaussian
#'  multiple linear regression model fitted for event date estimation and
#'  prediction.
#' @slot keep An [`integer`] vector giving the subscripts of the CA components
#'  to keep.
#' @section Extract:
#'  In the code snippets below, `x` is an `EventDate` object.
#'  \describe{
#'   \item{[`time(x)`][time()]}{Extract dates of assemblages.}
#'   \item{[`coef(x)`][coef()]}{Extract model coefficients.}
#'   \item{[`fitted(x)`][fitted()]}{Extract model fitted values.}
#'   \item{[`residuals(x)`][residuals()]}{Extract model residuals.}
#'   \item{[`sigma(x)`][sigma()]}{Extract the residual standard deviation.}
#'   \item{[`terms(x)`][terms()]}{Extract model terms.}
#'  }
#' @note
#'  Dates are internally stored as *[rata die][aion::RataDie-class]*.
#'  This class inherits from [`dimensio::CA-class`].
#' @seealso [`dimensio::CA-class`]
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases EventDate-class
#' @keywords internal
.EventDate <- setClass(
  Class = "EventDate",
  slots = c(
    contexts = "matrix",
    dates = "RataDie",
    model = "lm",
    keep = "integer"
  ),
  contains = "CA"
)

# AoristicSum ==================================================================
#' Aoristic Sum
#'
#' An S4 class to represent an aoristic analysis results.
#' @slot breaks A [`RataDie-class`] vector giving the date break between
#'  time-blocks.
#' @slot weights A [`numeric`] vector.
#' @slot groups A [`character`] vector to store the group names (if any).
#' @slot p A [`numeric`] [`array`] giving the aorisitic probabilities.
#' @section Coerce:
#'  In the code snippets below, `x` is an `AoristicSum` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @note
#'  This class inherits from [`aion::TimeSeries-class`]: dates are internally
#'  stored as *[rata die][aion::RataDie-class]*.
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases AoristicSum-class
#' @keywords internal
.AoristicSum <- setClass(
  Class = "AoristicSum",
  slots = c(
    breaks = "RataDie",
    weights = "numeric",
    groups = "character",
    p = "array"
  ),
  contains = "TimeSeries"
)

#' Rate of Change
#'
#' An S4 class to represent rates of change from an aoristic analysis.
#' @slot replicates A non-negative [`integer`] giving the number of
#'  replications.
#' @slot groups A [`character`] vector to store the group names (if any).
#' @section Coerce:
#'  In the code snippets below, `x` is an `AoristicSum` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @note
#'  This class inherits from [`aion::TimeSeries-class`]: dates are internally
#'  stored as *[rata die][aion::RataDie-class]*.
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases RateOfChange-class
#' @keywords internal
.RateOfChange <- setClass(
  Class = "RateOfChange",
  slots = c(
    replicates = "integer",
    groups = "character"
  ),
  contains = "TimeSeries"
)

# CountApportion ==================================================================
#' Count Apportioning
#'
#' An S4 class to represent an artifact apportioning results. Gives the
#' apportioning of artifact types (columns) per site (rows) and per period
#' (dim. 3).
#' @slot .Data An \eqn{m \times p \times k}{m x p x k} [`array`] giving the
#'  proportion of an artifact type (\eqn{p}) for a given period (\eqn{k}).
#' @slot p An \eqn{m \times p \times k}{m x p x k} [`array`] giving the
#'  probability of apportioning an artifact type (\eqn{p}) to a given period
#'  (\eqn{k}).
#' @slot method A [`character`] string specifying the distribution used for
#'  apportioning (type popularity curve).
#' @slot from A length-one [`numeric`] vector giving the beginning of the
#'  period of interest (in years AD).
#' @slot to A length-one [`numeric`] vector giving the end of the period of
#'  interest (in years AD).
#' @slot step A length-one [`numeric`] vector giving the step size, i.e. the
#'  width of each time step for apportioning (in years AD).
#' @note This class inherits from base [`array`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CountApportion-class
#' @keywords internal
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
#' @slot statistic A [`numeric`] vector giving the values of the t-statistic.
#' @slot parameter An [`integer`] giving the degrees of freedom for the
#'  t-statistic.
#' @slot p_value A [`numeric`] vector giving the the p-value for the test.
#' @section Coerce:
#'  In the code snippets below, `x` is an `IncrementTest` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @note
#'  This class inherits from [`aion::TimeSeries-class`]: dates are internally
#'  stored as *[rata die][aion::RataDie-class]*.
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases IncrementTest-class
#' @keywords internal
.IncrementTest <- setClass(
  Class = "IncrementTest",
  slots = c(
    statistic = "numeric",
    parameter = "integer",
    p_value = "numeric"
  ),
  contains = "TimeSeries"
)

# PermutationOrder =============================================================
#' Permutation Order
#'
#' S4 classes to represent a permutation order.
#' @slot rows_order An [`integer`] vector giving the rows permutation.
#' @slot columns_order An [`integer`] vector giving the columns permutation.
#' @section Subset:
#'  In the code snippets below, `x` is a `PermutationOrder` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extract information from a slot selected by subscript `i`.
#'   `i` is a length-one character vector.}
#'  }
#' @seealso [`dimensio::CA-class`]
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases PermutationOrder-class
#' @keywords internal
.PermutationOrder <- setClass(
  Class = "PermutationOrder",
  slots = c(
    rows_order = "integer",
    columns_order = "integer"
  ),
  contains = "VIRTUAL"
)

#' @rdname PermutationOrder-class
#' @aliases RankPermutationOrder-class
.RankPermutationOrder <- setClass(
  Class = "RankPermutationOrder",
  contains = "PermutationOrder"
)

#' @rdname PermutationOrder-class
#' @aliases AveragePermutationOrder-class
.AveragePermutationOrder <- setClass(
  Class = "AveragePermutationOrder",
  contains = c("PermutationOrder", "CA")
)

# RefineCA =====================================================================
#' Partial Bootstrap CA
#'
#' An S4 class to store partial bootstrap correspondence analysis results.
#' @slot length A [`numeric`] vector giving the convex hull maximum
#'  dimension length.
#' @slot cutoff A length-one [`numeric`] vector giving the cutoff value for
#'  samples selection.
#' @slot keep An [`integer`] vector giving the subscript of the variables
#'  to be kept.
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases RefineCA-class
#' @keywords internal
.RefinePermutationOrder <- setClass(
  Class = "RefinePermutationOrder",
  slots = c(
    length = "numeric",
    cutoff = "numeric",
    keep = "integer",
    margin = "integer"
  ),
  contains = "AveragePermutationOrder"
)
