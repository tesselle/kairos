# MEAN CERAMIC DATE
#' @include AllClasses.R AllGenerics.R
NULL

# MCD ==========================================================================
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

# Resample =====================================================================
#' @export
#' @rdname date_mcd
#' @aliases bootstrap,DateMCD-method
setMethod(
  f = "bootstrap",
  signature = signature(x = "DateMCD"),
  definition = function(x, level = 0.95, type = c("student", "normal"),
                        probs = c(0.25, 0.50, 0.75), n = 1000) {
    results <- apply(
      X = x,
      MARGIN = 1,
      FUN = arkhe::bootstrap,
      do = mcd,
      level = level,
      type = type,
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

# Plot =========================================================================
#' @export
#' @method autoplot DateMCD
autoplot.DateMCD <- function(object, ..., select = NULL, decreasing = TRUE) {
  ## Select data
  data <- as.data.frame(object)
  index <- select_by_indices(cases = data$samples, select = select)
  data <- data[index, ]

  ## Order data
  data <- data[order(data$dates), ]
  lvl <- unique(data$samples)
  lvl <- if (decreasing) rev(lvl) else lvl
  data$samples <- factor(data$samples, levels = lvl)

  ## ggplot2
  aes_point <- ggplot2::aes(x = .data$dates, y = .data$samples)

  ggplot2::ggplot(data = data) +
    ggplot2::geom_point(mapping = aes_point) +
    ggplot2::scale_x_continuous(name = "Mean Ceramic Date (year CE)") +
    ggplot2::scale_y_discrete(name = "Assemblage")
}

#' @export
#' @rdname plot_mcd
setMethod("autoplot", "DateMCD", autoplot.DateMCD)

#' @export
#' @rdname plot_mcd
#' @aliases plot,DateMCD,missing-method
setMethod(
  f = "plot",
  signature = c(x = "DateMCD", y = "missing"),
  definition = function(x, select = NULL, decreasing = TRUE) {
    gg <- autoplot(object = x, select = select, decreasing = decreasing) +
      ggplot2::theme_bw()
    print(gg)
    invisible(x)
  }
)
