# HELPERS

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
  if (window %% 2 == 0)
    stop(sprintf("%s must be an odd integer.", sQuote("window")), call. = FALSE)

  n <- if (is.matrix(x) || is.data.frame(x)) nrow(x) else length(x)
  i <- seq_len(n) # Indices of the rows

  ## Matrix of rolling-window indices of length w
  w <- stats::embed(i, window)[, window:1]
  inds <- as.vector(t(w)) # Flatten indices

  ## Window mid-point
  m <- w[, ceiling(window / 2)]

  list(i = inds, w = rep(m, each = window))
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
