# SIMULATE
#' @include AllGenerics.R
NULL

# MeanDate =====================================================================
#' @export
#' @method simulate MeanDate
simulate.MeanDate <- function(object, nsim = 1000, seed = NULL, ...) {

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
#' @rdname simulate.MeanDate
#' @aliases simulate,MeanDate-method
setMethod("simulate", c(object = "MeanDate"), simulate.MeanDate)

#' Resample
#'
#' Simulates observations from a multinomial distribution.
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param do A [`function`] that takes `x` as an argument
#'  and returns a single numeric value.
#' @param n A non-negative [`integer`] specifying the number of bootstrap
#'  replications.
#' @param size A non-negative [`integer`] specifying the sample size.
#' @param ... Extra arguments passed to `do`.
#' @return
#'  The `n` values of `do`.
#' @seealso [stats::rmultinom()]
#' @keywords internal
#' @noRd
resample <- function(x, do, n, size = sum(x), ...) {
  prob <- x / sum(x)
  replicates <- stats::rmultinom(n, size = size, prob = prob)
  apply(X = replicates, MARGIN = 2, FUN = do, ...)
}
