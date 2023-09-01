# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}

make_par <- function(params, x, n = 0) {
  p <- params[[x]] %||% graphics::par()[[x]]
  if (n > 0) p <- rep(p, length.out = n)
  p
}

#' Compute x Limits
#'
#' Computes x limits for a time series according to a given calendar.
#' This ensures that the x axis is always in chronological order.
#' @param x A [`TimeSeries-class`] object.
#' @param calendar A [`TimeScale-class`] object.
#' @return A length-two [`numeric`] vector.
#' @keywords internal
#' @noRd
xlim <- function(x, calendar) {
  x <- range(time(x, calendar = NULL))
  if (is.null(calendar)) return(x)
  as_year(x, calendar = calendar)
}

#' Plotting Dimensions of Character Strings
#'
#' Convert string length in inch to number of (margin) lines.#'
#' @param x A [`character`] vector of string whose length is to be calculated.
#' @param ... Further parameter to be passed to [graphics::strwidth()]`, such as
#'  `cex`
#' @return
#'  A [`numeric`] vector (maximum string width in units of margin lines).
#' @keywords internal
#' @noRd
inch2line <- function(x, ...) {
  (max(graphics::strwidth(x, units = "inch", ...)) /
     graphics::par("cin")[2] + graphics::par("mgp")[2]) * graphics::par("cex")
}

#' Indices of a rolling window
#'
#' @param x An object.
#' @param window An [`integer`] scalar giving the window size.
#' @return A [`list`] with the following components:
#'  \describe{
#'   \item{i}{An [`integer`] vector of indices.}
#'   \item{w}{An [`integer`] vector of indices giving the indice of the window
#'   mid-point.}
#'  }
#' @keywords internal
#' @noRd
roll <- function(x, window = 3) {
  ## Validation
  arkhe::assert_odd(window)

  n <- if (is.matrix(x) || is.data.frame(x)) nrow(x) else length(x)
  i <- seq_len(n) # Indices of the rows

  ## Matrix of rolling-window indices of length w
  w <- stats::embed(i, window)[, window:1]
  inds <- as.vector(t(w)) # Flatten indices

  ## Window mid-point
  m <- w[, ceiling(window / 2)]

  list(i = inds, w = rep(m, each = window))
}

#' Resample
#'
#' Simulates observations from a multinomial distribution.
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param do A [`function`] that takes `x` as an argument
#'  and returns a single numeric value.
#' @param n A non-negative [`integer`] specifying the number of bootstrap
#'  replications.
#' @param size A non-negative [`integer`] specifying the sample size.
#' @param f A [`function`] that takes a single numeric vector (the result of
#'  `do`) as argument.
#' @param ... Extra arguments passed to `do`.
#' @return
#'  If `f` is `NULL`, `resample()` returns the `n` values of `do`. Else,
#'  returns the result of `f` applied to the `n` values of `do`.
#' @seealso [stats::rmultinom()]
#' @keywords internal
#' @noRd
resample <- function(x, do, n, size = sum(x), ..., f = NULL) {
  ## Validation
  arkhe::assert_count(x)

  prob <- x / sum(x)
  replicates <- stats::rmultinom(n, size = size, prob = prob)
  values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)
  if (is.function(f)) values <- f(values)
  values
}

#' Quantiles of a Density Estimate
#'
#' @param x A [`numeric`] vector giving the n coordinates of the points where
#'  the density is estimated.
#' @param y A `numeric` [`matrix`] of density estimates (each column is a
#'  density estimate).
#' @param probs A [`numeric`] vector of probabilities with values in
#'  \eqn{[0,1]}.
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @param ... Currently not used.
#' @return
#'  A `numeric` [`matrix`] containing the quantiles.
#' @keywords internal
#' @noRd
quantile_density <- function(x, y, probs = seq(0, 1, 0.25), na.rm = FALSE, ...) {
  eps <- 100 * .Machine$double.eps
  if (anyNA(probs) | any(probs < -eps | probs > 1 + eps))
    stop(sprintf("%s outside [0,1]", sQuote("probs")))

  q <- apply(
    X = y,
    MARGIN = 2,
    FUN = function(y, x, probs, na.rm) {
      np <- length(probs)
      qs <- rep(NA_real_, np)
      if (na.rm) {
        i <- !is.na(x) & !is.na(y)
        x <- x[i]
        y <- y[i]
      }
      if (np > 0) {
        nn <- length(x)
        Fx <- cumsum(y * c(0, diff(x)))
        Fx <- Fx / Fx[nn]
        for (j in seq_len(np)) {
          ii <- min(which(Fx >= probs[j]))
          if (!is.na(ii) && ii >= 1 && ii <= nn) qs[j] <- x[ii]
        }
        qs
      }
    },
    x = x,
    probs = probs,
    na.rm = na.rm
  )

  if (!is.null(dim(q))) {
    q <- t(q)
    colnames(q) <- paste0(round(probs * 100, digits = 0), "%")
  }
  q
}
