# FREQUENCY INCREMENT TEST
#' @include AllGenerics.R AllClasses.R
NULL

# FIT ==========================================================================
#' @export
#' @rdname fit
#' @aliases fit,data.frame,numeric-method
setMethod(
  f = "fit",
  signature = c(object = "data.frame", dates = "numeric"),
  definition = function(object, dates, calendar = CE(),
                        level = 0.95, roll = FALSE, window = 3) {
    object <- data.matrix(object)
    methods::callGeneric(object, dates = dates, calendar = calendar,
                         level = level, roll = roll, window = window)
  }
)

#' @export
#' @rdname fit
#' @aliases fit,matrix,numeric-method
setMethod(
  f = "fit",
  signature = c(object = "matrix", dates = "numeric"),
  definition = function(object, dates, calendar = CE(),
                        level = 0.95, roll = FALSE, window = 3) {
    ## Validation
    alpha <- 1 - level
    half_window <- (window - 1) / 2
    arkhe::assert_length(dates, nrow(object))

    ## Convert to rata die
    if (is.null(calendar)) {
      dates <- aion::as_fixed(dates)
    } else {
      dates <- aion::fixed(dates, calendar = calendar)
    }

    ## Compute test
    results <- test_FIT(object, dates, roll = FALSE)
    results <- results[[1L]]

    ## Check results
    failed <- is.na(results$p.value)
    if (any(failed)) {
      msg <- sprintf("%d elements were skipped:\n%s", sum(failed),
                     paste0("* ", rownames(results)[failed], collapse = "\n"))
      warning(msg, call. = FALSE)
    }

    ## Rolling window
    fit_roll <- matrix(FALSE, nrow = nrow(object), ncol = ncol(object))
    if (roll) {
      results_roll <- test_FIT(object, dates, roll = roll, window = window)
      results_roll <- lapply(
        X = results_roll,
        FUN = function(x, alpha) { x$p.value <= alpha },
        alpha = alpha
      )
      fit_row <- seq(from = half_window + 1, to = nrow(fit_roll) - half_window)
      fit_roll[fit_row, ] <- do.call(rbind, results_roll)

      fit_roll <- apply(
        X = fit_roll,
        MARGIN = 2,
        FUN = function(x, half_window) {
          vapply(
            X = seq_along(x),
            FUN = function(x, y, k) {
              max <- length(y)
              lower <- x - k
              lower[lower < 1] <- 1
              upper <- x + k
              upper[upper > max] <- max
              any(y[lower:upper], na.rm = TRUE)
            },
            FUN.VALUE = logical(1),
            y = x,
            k = half_window
          )
        },
        half_window = half_window
      )
    }

    ## Build array
    select <- results$p.value <= alpha
    x <- y <- z <- object
    x[, !select] <- NA
    y[, select] <- NA
    z[!fit_roll] <- NA

    res <- array(
      data = c(z, x, y),
      dim = c(dim(object), 3),
      dimnames = c(dimnames(object), list(c("roll", "selection", "neutral")))
    )

    ts <- aion::series(object = res, time = dates)
    .IncrementTest(
      ts,
      statistic = results$t,
      parameter = 1L,
      p_value = results$p.value
    )
  }
)

#' @param x A \eqn{m x p} [`numeric`] matrix.
#' @param time A length-\eqn{m} [`numeric`] vector.
#' @param roll A [`logical`] scalar.
#' @param window An [`integer`].
#' @return A list of \eqn{p x 3} [`data.frame`].
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
test_FIT <- function(x, time, roll = FALSE, window = 3, ...) {
  ## Prepare data
  time <- as.numeric(time)

  ## Compute frequency as ratio of count of type of interest to all other types
  count_others <- lapply(
    X = seq_len(ncol(x)),
    FUN = function(i, data) { rowSums(data[, -i]) },
    data = x
  )
  freq <- x / do.call(cbind, count_others)

  ## Compute test
  if (!roll) {
    roll_list <- list(seq_len(nrow(freq)))
  } else {
    roll_index <- roll(freq, window = window)
    roll_list <- split(x = roll_index[["i"]], f = roll_index[["w"]])
  }

  k <- 1
  results <- vector(mode = "list", length = length(roll_list))
  for (i in roll_list) {
    roll_freq <- freq[i, ]
    roll_time <- time[i]
    roll_date <- roll_time[ceiling(window / 2)]

    fit <- apply(
      X = roll_freq,
      MARGIN = 2,
      FUN = function(v, t, ...) {
        tryCatch(
          error = function(cnd) c(t = NA, p.value = NA),
          { FIT(v, t, ...) }
        )
      },
      t = roll_time,
      ...
    )

    results[[k]] <- data.frame(
      column = colnames(fit),
      time = roll_date,
      t(fit),
      row.names = NULL
    )
    k <- k + 1
  }

  return(results)
}

#' Frequency Increment Test (FIT)
#'
#' @param v A [`numeric`] vector of frequencies.
#' @param t A [`numeric`] vector of time coordinates.
#' @param ... Extra parameter passed to [stats::t.test()].
#' @return A [`numeric`] vector containing the following components:
#'  \describe{
#'   \item{`t`}{the value of the test statistic.}
#'   \item{`p.value`}{the p-value for the test.}
#'  }
#' @author N. Frerebeau
#' @references
#'  Feder, A. F., Kryazhimskiy, S. & Plotkin, J. B. (2014). Identifying
#'  Signatures of Selection in Genetic Time Series. *Genetics*, 196(2),
#'  509-522. \doi{10.1534/genetics.113.158220}.
#' @keywords internal
#' @noRd
FIT <- function(v, t, ...) {
  ## Validation
  arkhe::assert_type(v, "numeric")
  arkhe::assert_type(t, "numeric")
  arkhe::assert_length(t, length(v))

  ## Remove zeros
  index <- v > 0
  v <- v[index]
  t <- t[index]
  if (length(t) < 3)
    stop("A minimum of three time points is needed.", call. = FALSE)

  ## Rescaled allele-frequency increments
  Y <- diff(v, lag = 1) /
    sqrt(2 * utils::head(v, -1) * (1 - utils::head(v, -1)) * diff(t, lag = 1))

  ## Statistics
  t_test <- stats::t.test(Y, ...)
  c(t = as.numeric(t_test$statistic), p.value = t_test$p.value)
}

# Plot =========================================================================
#' @export
#' @method plot IncrementTest
plot.IncrementTest <- function(x, calendar = getOption("kairos.calendar"),
                               col.neutral = "#004488", col.selection = "#BB5566",
                               col.roll = "grey", flip = FALSE, ncol = NULL,
                               xlab = NULL, ylab = NULL,
                               main = NULL, sub = NULL,
                               ann = graphics::par("ann"), axes = TRUE,
                               frame.plot = axes, ...) {
  ## Panel
  panel_fit <- function(x, y, ...) {
    graphics::lines(x, y, ...)
    graphics::points(x, y, ...)
  }

  ## Method for TimeSeries
  methods::callNextMethod(x = x, calendar = calendar, panel = panel_fit,
                          flip = flip, ncol = ncol,
                          xlab = xlab, ylab = ylab,
                          main = main, sub = sub,
                          ann = ann, axes = axes,
                          frame.plot = frame.plot,
                          col = c(col.roll, col.selection, col.neutral),
                          lwd = c(10, 1, 1), pch = 16, cex = 1, ...)
}

#' @export
#' @rdname plot_fit
#' @aliases plot,IncrementTest,missing-method
setMethod("plot", c(x = "IncrementTest", y = "missing"), plot.IncrementTest)
