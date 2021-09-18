# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# MeanDate ======================================================================
setValidity(
  Class = "MeanDate",
  method = function(object) {
    # Get data
    types <- object@types
    weights <- object@weights
    m <- nrow(weights)
    p <- ncol(weights)

    cnd <- list(
      arkhe::validate(arkhe::assert_length(object, m)),
      arkhe::validate(arkhe::assert_length(types, p))
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
    cutoff <- object@cutoff
    keep <- object@keep

    cnd <- list(
      arkhe::validate(arkhe::assert_length(dates, nrow(data))),
      arkhe::validate(arkhe::assert_scalar(cutoff, "integer"))
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
    from <- object@from
    to <- object@to
    weights <- object@weights
    groups <- object@groups
    dates <- object@dates
    p <- object@p

    i <- nrow(p)
    j <- nrow(object)
    k <- length(unique(groups))

    cnd <- list(
      arkhe::validate(arkhe::assert_length(from, i)),
      arkhe::validate(arkhe::assert_length(to, i)),
      arkhe::validate(arkhe::assert_length(weights, i)),
      arkhe::validate(arkhe::assert_length(groups, i)),
      arkhe::validate(arkhe::assert_length(dates, j)),
      arkhe::validate(arkhe::assert_dimensions(p, c(i, j, k)))
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
    blocks <- object@blocks
    groups <- object@groups

    i <- nrow(object)
    j <- ncol(object)
    k <- dim(groups)[[3]]

    cnd <- list(
      arkhe::validate(arkhe::assert_scalar(replicates, "integer")),
      arkhe::validate(arkhe::assert_length(groups, k)),
      arkhe::validate(arkhe::assert_length(blocks, j))
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
      arkhe::validate(arkhe::assert_scalar(from, "numeric")),
      arkhe::validate(arkhe::assert_scalar(to, "numeric")),
      arkhe::validate(arkhe::assert_scalar(method, "character")),
      arkhe::validate(arkhe::assert_dimensions(p, dim(object)))
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
    counts <- object@counts
    dates <- object@dates
    statistic <- object@statistic
    parameter <- object@parameter
    p_value <- object@p_value

    i <- nrow(counts)
    j <- ncol(counts)

    cnd <- list(
      arkhe::validate(arkhe::assert_length(dates, i)),
      arkhe::validate(arkhe::assert_length(statistic, j)),
      arkhe::validate(arkhe::assert_scalar(parameter, "integer")),
      arkhe::validate(arkhe::assert_length(p_value, j))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)
