# FREQUENCY INCREMENT TEST
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname fit
#' @aliases fit,data.frame,numeric-method
setMethod(
  f = "fit",
  signature = signature(object = "data.frame", dates = "numeric"),
  definition = function(object, dates) {
    object <- data.matrix(object)
    methods::callGeneric(object, dates = dates)
  }
)

#' @export
#' @rdname fit
#' @aliases fit,matrix,numeric-method
setMethod(
  f = "fit",
  signature = signature(object = "matrix", dates = "numeric"),
  definition = function(object, dates) {
    ## Validation
    arkhe::assert_length(dates, nrow(object))

    ## Compute test
    results <- testFIT(object, dates, roll = FALSE)[[1L]]

    ## Check results
    failed <- is.na(results$p.value)
    if (any(failed)) {
      msg <- sprintf("%d elements were skipped:\n%s", sum(failed),
                     paste0("* ", rownames(results)[failed], collapse = "\n"))
      warning(msg, call. = FALSE)
    }

    .IncrementTest(
      counts = object,
      dates = dates,
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
testFIT <- function(x, time, roll = FALSE, window = 3, ...) {
  ## Prepare data
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
      dates = roll_date,
      t(fit)
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
