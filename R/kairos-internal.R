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

#' Subset
#'
#' Subset a matrix by matching names.
#' @param x A [`matrix`].
#' @param y A named [`vector`].
#' @return A [`matrix`].
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
bind_by_names <- function(x, y) {
  if (!is.null(names(y)) && !is.null(rownames(x))) {
    z <- merge(
      x = data.frame(y),
      y = as.data.frame(x),
      by = 0,
      all.x = FALSE,
      all.y = TRUE,
      sort = FALSE
    )
    z <- z[, -c(1)] # Remove extra column from merge
  } else if (nrow(x) == length(y)) {
    z <- data.frame(y, x)
  } else {
    stop("Names are missing.", call. = FALSE)
  }
}

#' Select
#'
#' Select data.
#' @param cases A vector.
#' @param select A vector.
#' @return An [`integer`] vector of indices.
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
select_by_indices <- function(cases, select = NULL) {
  if (is.null(select)) {
    index <- seq_along(cases)
  } else if (is.character(select)) {
    index <- which(select == cases)
  } else {
    index <- as.integer(select)
  }
  if (length(index) == 0) stop("Wrong selection.", call. = FALSE)
  index
}

#' Build a Long Data Frame
#'
#' Stacks vector from a [`data.frame`].
#' @param x A [`matrix`] or [`data.frame`]
#' @param value A [`character`] string specifying the name of the
#'  column containing the result of concatenating `x`.
#' @param factor A [`logical`] scalar: should row and columns names be
#'  coerced to factors? The default (`TRUE`) preserves the original
#'  ordering.
#' @return A [`data.frame`] withe the following variables:
#'  "`case`", "`value`" and "`type`".
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
wide2long <- function(x, value = "data", factor = TRUE) {
  x <- as.data.frame(x)
  row_names <- rownames(x)
  col_names <- rownames(x)

  stacked <- utils::stack(x)
  long <- cbind.data.frame(stacked, row_names)
  colnames(long) <- c(value, "type", "case")
  if (factor) {
    # Preserves the original ordering of the rows and columns
    long$case <- factor(long$case, levels = unique(long$case))
    long$type <- factor(long$type, levels = unique(long$type))
  }
  long
}
