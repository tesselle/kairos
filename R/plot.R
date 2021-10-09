# PLOT LINE
#' @include AllClasses.R AllGenerics.R
NULL

# CountMatrix ==================================================================
#' @export
#' @rdname plot_time
#' @aliases plot_time,CountMatrix,missing-method
setMethod(
  f = "plot_time",
  signature = signature(object = "CountMatrix", date = "missing"),
  definition = function(object, facet = FALSE) {
    dates <- get_dates(object)
    methods::callGeneric(object, dates, facet = facet)
  }
)

#' @export
#' @rdname plot_time
#' @aliases plot_time,CountMatrix,numeric-method
setMethod(
  f = "plot_time",
  signature = signature(object = "CountMatrix", date = "numeric"),
  definition = function(object, dates, facet = FALSE) {
    ## Validation
    arkhe::assert_type(dates, "numeric")
    arkhe::assert_length(dates, nrow(object))

    ## Prepare data
    data <- prepare_time(object, dates)

    ## ggplot
    if (facet) {
      facet <- ggplot2::facet_wrap(
        facets = ggplot2::vars(.data$column),
        scales = "free_y"
      )
      aes_plot <- ggplot2::aes(x = .data$x, y = .data$y)
    } else {
      facet <- NULL
      aes_plot <- ggplot2::aes(x = .data$x, y = .data$y, colour = .data$column)
    }

    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(name = "Time") +
      ggplot2::scale_y_continuous(name = "Frequency") +
      facet
  }
)

# IncrementTest ================================================================
#' @export
#' @method autoplot IncrementTest
autoplot.IncrementTest <- function(object, ..., level = 0.95, roll = FALSE,
                                   window = 3) {
  ## Prepare data
  alpha <- 1 - level
  counts <- object[["counts"]]
  dates <- object[["dates"]]
  data <- prepare_time(counts, dates)

  signature_fit <- as.data.frame(object)
  signature_fit$signature <- ifelse(signature_fit$p.value <= alpha,
                                    "selection", "neutral")

  data <- merge(x = data, y = signature_fit, by.x = "column", by.y = 0,
                all.x = TRUE, all.y = FALSE)

  if (roll) {
    roll_fit <- testFIT(counts, dates, roll = roll, window = window)

    roll_fit <- do.call(rbind.data.frame, roll_fit)
    roll_fit$signature <- roll_fit$p.value <= alpha

    data <- merge(x = data, y = roll_fit, by = c("column", "dates"),
                  all.x = TRUE, all.y = FALSE, sort = TRUE,
                  suffixes = c("","_roll"))

    data <- by(
      data,
      INDICES = data$column,
      FUN = function(x, half_window) {
        x$signature_sub <- vapply(
          X = seq_along(x$signature_roll),
          FUN = function(x, y, k) {
            max <- length(y)
            lower <- x - k
            lower[lower < 1] <- 1
            upper <- x + k
            upper[upper > max] <- max
            any(y[lower:upper], na.rm = TRUE)
          },
          FUN.VALUE = logical(1),
          y = x$signature_roll,
          k = half_window
        )
        x
      },
      half_window = (window - 1) / 2
    )
    data <- do.call(rbind.data.frame, data)

    gg_roll <- ggplot2::geom_line(
      data = data[data$signature_sub, ],
      mapping = ggplot2::aes(group = .data$column),
      size = 5, colour = "grey80", lineend = "round"
    )
  } else {
    gg_roll <- NULL
  }

  ## ggplot
  ggplot2::ggplot(data = data) +
    ggplot2::aes(x = .data$x, y = .data$y, colour = .data$signature) +
    gg_roll +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(name = "Time") +
    ggplot2::scale_y_continuous(name = "Frequency") +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(.data$column),
      scales = "free_y"
    )
}

#' @export
#' @rdname plot_fit
#' @aliases autoplot,IncrementTest-method
setMethod("autoplot", "IncrementTest", autoplot.IncrementTest)

#' @export
#' @method plot IncrementTest
plot.IncrementTest <- function(x, level = 0.95, roll = FALSE, window = 3, ...) {
  gg <- autoplot(object = x, level = level, roll = roll, window = window) +
    ggplot2::theme_bw()
  print(gg)
  invisible(x)
}

#' @export
#' @rdname plot_fit
#' @aliases plot,IncrementTest,missing-method
setMethod("plot", c(x = "IncrementTest", y = "missing"), plot.IncrementTest)

# Prepare data =================================================================
## Build a long table for ggplot2
## * Must return a data.frame
## * Must preserve original ordering
prepare_time <- function(object, dates) {
  data <- arkhe::as_long(object, factor = TRUE)

  data$x <- data$dates <- dates
  data$y <- data$value

  ## Remove zeros in case of log scale
  data <- data[data$value > 0, ]

  return(data)
}
