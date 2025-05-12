#' Deprecated Functions in kairos
#'
#' These functions still work but will be removed (defunct) in the next version.
#' @name kairos-deprecated
#' @keywords internal
NULL

.SimulationMeanDate <- setClass(
  Class = "SimulationMeanDate",
  slots = c(
    replications = "matrix",
    seed = "numeric"
  ),
  contains = "MeanDate"
)

#' @export
#' @method simulate MeanDate
simulate.MeanDate <- function(object, nsim = 1000, seed = NULL, ...) {
  .Deprecated(new = "bootstrap()", old = "simulate()")

  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    stats::runif(1)
  if (is.null(seed))
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  results <- apply(
    X = object[, , 1, drop = TRUE],
    MARGIN = 1,
    FUN = resample,
    do = .mcd,
    n = nsim,
    dates = object@dates
  )

  .SimulationMeanDate(object, replications = t(results), seed = RNGstate)
}

#' @export
#' @rdname kairos-deprecated
#' @aliases simulate,MeanDate-method
setMethod("simulate", c(object = "MeanDate"), simulate.MeanDate)

resample <- function(x, do, n, size = sum(x), ...) {
  prob <- x / sum(x)
  replicates <- stats::rmultinom(n, size = size, prob = prob)
  apply(X = replicates, MARGIN = 2, FUN = do, ...)
}

#' @export
#' @rdname kairos-deprecated
setGeneric(
  name = "get_order",
  def = function(x, ...) standardGeneric("get_order")
)

#' @export
#' @rdname kairos-deprecated
setMethod(
  f = "get_order",
  signature = c("PermutationOrder"),
  definition = function(x, margin = c(1, 2)) {
    .Deprecated(new = "order_rows()/order_columns()", old = "get_order()")
    o <- list(rows = integer(0), columns = integer(0))
    o$rows <- if (1 %in% margin) x@rows_order else NULL
    o$columns <- if (2 %in% margin) x@columns_order else NULL
    if (length(o) == 1) unname(unlist(o)) else o
  }
)

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

#' @rdname kairos-deprecated
#' @aliases seriate_refine-method
setGeneric(
  name = "seriate_refine",
  def = function(object, ...) standardGeneric("seriate_refine")
)

#' @export
#' @rdname kairos-deprecated
#' @aliases seriate_refine,AveragePermutationOrder-method
setMethod(
  f = "seriate_refine",
  signature = c(object = "AveragePermutationOrder"),
  definition = function(object, cutoff, margin = 1, axes = 1, n = 30, ...) {
    ## Partial bootstrap CA
    ## /!\ Be careful: AveragePermutationOrder inherits from CA
    object <- dimensio::bootstrap(object, n = n)
    methods::callGeneric(object, cutoff = cutoff, margin = margin, axes = axes, ...)
  }
)

#' @export
#' @rdname kairos-deprecated
#' @aliases seriate_refine,BootstrapCA-method
setMethod(
  f = "seriate_refine",
  signature = c(object = "BootstrapCA"),
  definition = function(object, cutoff, margin = 1, axes = 1, ...) {
    .Deprecated(new = "refine", old = "seriate_refine()")
    ## Validation
    arkhe::assert_function(cutoff)

    ca_rep <- dimensio::get_replications(object, margin = margin)

    ## Compute convex hull
    hull <- apply(
      X = ca_rep,
      MARGIN = 1,
      FUN = function(x, axes) compute_chull(t(x), c(1, 2)),
      axes = axes
    )

    ## Get convex hull maximal dimension length for each sample
    len <- vapply(
      X = hull,
      FUN = function(x) max(stats::dist(x, method = "euclidean")),
      FUN.VALUE = double(1)
    )

    ## Get cutoff values
    limit <- cutoff(len)

    ## Samples to be excluded
    sup <- which(len >= limit)

    ## Seriation
    ser <- seriate_average(dimensio::get_data(object), margin = c(1, 2),
                           axes = axes, sup_row = sup, ...)

    .RefinePermutationOrder(
      ser,
      length = len,
      cutoff = limit,
      keep = which(len < limit),
      margin = as.integer(margin)
    )
  }
)

#' @export
#' @method hist RefinePermutationOrder
hist.RefinePermutationOrder <- function(x, ...) {
  ## Get data
  cutoff <- x@cutoff

  graphics::hist(
    x = x@length,
    xlab = tr_("Maximum length"),
    main = NULL,
    las = 1,
    ...
  )
  graphics::abline(v = cutoff, col = "red")
  graphics::axis(side = 3, at = cutoff, labels = round(cutoff, 2), tick = FALSE,
                 col = "red", col.ticks = "red", col.axis = "red")

  invisible(x)
}

#' @export
#' @rdname kairos-deprecated
# @describeIn seriate_refine Compute and plot a histogram of convex hull
#  maximum dimension length.
#' @aliases hist,RefinePermutationOrder-method
setMethod("hist", "RefinePermutationOrder", hist.RefinePermutationOrder)
