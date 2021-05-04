# STATISTICS
#' @include AllClasses.R AllGenerics.R
NULL

#' Jackknife Estimation
#'
#' @param x A vector.
#' @param do A [`function`] that takes `x` as an argument and returns a single
#'  numeric value.
#' @param ... Extra arguments passed to `do`.
#' @return A `numeric` vector with the following elements:
#'  \describe{
#'   \item{`values`}{The \eqn{n} leave-one-out values.}
#'   \item{`mean`}{The jackknife estimate of mean.}
#'   \item{`bias`}{The jackknife estimate of bias.}
#'   \item{`error`}{he jackknife estimate of standard error.}
#'  }
#' @keywords internal
stats_jackknife <- function(x, do, ...) {
  n <- length(x)
  hat <- do(x, ...)

  jack_values <- vapply(
    X = seq_len(n),
    FUN = function(i, x, do, ...) {
      do(x[-i], ...)
    },
    FUN.VALUE = double(1),
    x, do, ...
  )

  jack_mean <- mean(jack_values)
  jack_bias <- (n - 1) * (jack_mean - hat)
  jack_error <- sqrt(((n - 1) / n) * sum((jack_values - jack_mean)^2))

  results <- c(jack_mean, jack_bias, jack_error)
  names(results) <- c("mean", "bias", "error")
  results
}

#' Bootstrap Estimation
#'
#' @param x A vector.
#' @param do A [`function`] that takes `x` as an argument
#'  and returns a single numeric value.
#' @param probs A [`numeric`] vector of probabilities with values in
#'  \eqn{[0,1]} (see [stats::quantile()]).
#' @param n A non-negative [`integer`] giving the number of bootstrap
#' replications.
#' @param na.rm A [`logical`] scalar: should missing values be removed
#' from `x` before the quantiles are computed?
#' @param ... Extra arguments passed to `do`.
#' @return A `numeric` vector with the following elements:
#'  \describe{
#'   \item{`min`}{Minimum value.}
#'   \item{`mean`}{Mean value.}
#'   \item{`max`}{Maximum value.}
#'   \item{`Q*`}{Sample quantile to * probability.}
#'  }
#' @keywords internal
stats_bootstrap <- function(x, do, probs = c(0.05, 0.95),
                            n = 1000, na.rm = FALSE, ...) {
  total <- sum(x)
  replicates <- stats::rmultinom(n, size = total, prob = x / total)
  boot_values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)

  Q <- stats::quantile(boot_values, probs = probs, na.rm = na.rm, names = FALSE)
  quant <- paste0("Q", round(probs * 100, 0))

  results <- c(
    min(boot_values, na.rm = na.rm),
    mean(boot_values, na.rm = na.rm),
    max(boot_values, na.rm = na.rm),
    Q
  )
  names(results) <- c("min", "mean", "max", quant)
  results
}
