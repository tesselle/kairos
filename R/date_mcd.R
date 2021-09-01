# MEAN CERAMIC DATE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname date_mcd
#' @aliases date_mcd,CountMatrix,numeric-method
setMethod(
  f = "date_mcd",
  signature = signature(object = "CountMatrix", dates = "numeric"),
  definition = function(object, dates) {
    ## Validation
    arkhe::assert_length(dates, ncol(object))

    ## Calculate MCD
    mcd_dates <- apply(
      X = object,
      MARGIN = 1,
      FUN = mcd,
      dates = dates
    )

    .DateMCD(
      object,
      dates_types = dates,
      dates_mcd = mcd_dates
    )
  }
)

#' @export
#' @rdname date_mcd
#' @aliases bootstrap,DateMCD-method
setMethod(
  f = "bootstrap",
  signature = signature(x = "DateMCD"),
  definition = function(x, probs = c(0.05, 0.95), n = 1000) {
    results <- apply(
      X = x,
      MARGIN = 1,
      FUN = arkhe::bootstrap,
      do = mcd,
      probs = probs,
      n = n,
      dates = x@dates_types
    )
    as.data.frame(t(results))
  }
)

#' @export
#' @rdname date_mcd
#' @aliases jackknife,DateMCD-method
setMethod(
  f = "jackknife",
  signature = signature(x = "DateMCD"),
  definition = function(x) {
    results <- apply(
      X = x,
      MARGIN = 1,
      FUN = function(x, y) {
        n <- length(x)
        hat <- mcd(x, y)

        jack_values <- vapply(
          X = seq_len(n),
          FUN = function(i, x, y) {
            mcd(x[-i], y[-i])
          },
          FUN.VALUE = double(1),
          x, y
        )

        jack_mean <- mean(jack_values)
        jack_bias <- (n - 1) * (jack_mean - hat)
        jack_error <- sqrt(((n - 1) / n) * sum((jack_values - jack_mean)^2))

        results <- c(jack_mean, jack_bias, jack_error)
        names(results) <- c("mean", "bias", "error")
        results
      },
      y = x@dates_types
    )
    as.data.frame(t(results))
  }
)

#' Mean Ceramic Date
#'
#' @param counts A [`numeric`] vector.
#' @param dates A [`numeric`] vector.
#' @param na.rm A [`logical`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @return A [`numeric`] value.
#' @keywords internal
#' @noRd
mcd <- function(counts, dates, na.rm = FALSE) {
  ## Calculate relative frequencies
  freq <- counts / sum(counts, na.rm = na.rm)

  ## Calculate date
  dates_mcd <- sum(freq * dates, na.rm = na.rm)

  dates_mcd
}
