# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# MeanDate ======================================================================
setValidity(
  Class = "MeanDate",
  method = function(object) {
    # Get data
    dates <- object@dates
    m <- nrow(weights)
    p <- ncol(object)

    cnd <- list(
      arkhe::validate(arkhe::assert_length(dates, p))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)

# EventDate ====================================================================
setValidity(
  Class = "EventDate",
  method = function(object) {
    # Get data
    data <- object@data
    dates <- object@dates
    model <- object@model
    keep <- object@keep

    cnd <- list(
      arkhe::validate(arkhe::assert_length(dates, nrow(data)))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)

# AoristicSum ==================================================================
setValidity(
  Class = "AoristicSum",
  method = function(object) {
    # Get data
    span <- object@span
    groups <- object@groups
    breaks <- object@breaks
    p <- object@p

    i <- nrow(p)
    j <- nrow(object)
    k <- length(unique(groups))

    cnd <- list(
      arkhe::validate(arkhe::assert_length(span, i)),
      arkhe::validate(arkhe::assert_length(groups, i)),
      arkhe::validate(arkhe::assert_length(breaks, j + 1)),
      arkhe::validate(arkhe::assert_dim(p, c(i, j, k)))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)

# RateOfChange =================================================================
setValidity(
  Class = "RateOfChange",
  method = function(object) {
    # Get data
    replicates <- object@replicates
    groups <- object@groups

    i <- nrow(object)
    j <- ncol(object)
    k <- dim(groups)[[3]]

    cnd <- list(
      arkhe::validate(arkhe::assert_length(replicates, 1)),
      arkhe::validate(arkhe::assert_length(groups, k))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)

# CountApportion ===============================================================
setValidity(
  Class = "CountApportion",
  method = function(object) {
    # Get data
    p <- object@p
    method <- object@method
    from <- object@from
    to <- object@to
    step <- object@step

    cnd <- list(
      arkhe::validate(arkhe::assert_length(from, 1)),
      arkhe::validate(arkhe::assert_length(to, 1)),
      arkhe::validate(arkhe::assert_length(method, 1)),
      arkhe::validate(arkhe::assert_dim(p, dim(object)))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)

# IncrementTest ================================================================
setValidity(
  Class = "IncrementTest",
  method = function(object) {
    # Get data
    statistic <- object@statistic
    parameter <- object@parameter
    p_value <- object@p_value

    j <- ncol(object)

    cnd <- list(
      arkhe::validate(arkhe::assert_length(statistic, j)),
      arkhe::validate(arkhe::assert_length(parameter, 1)),
      arkhe::validate(arkhe::assert_length(p_value, j))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)
