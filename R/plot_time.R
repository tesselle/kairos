# PLOT LINE
#' @include AllClasses.R AllGenerics.R
NULL

# CountMatrix ==================================================================
#' @export
#' @rdname plot_time
#' @aliases plot_time,CountMatrix-method
setMethod(
  f = "plot_time",
  signature = signature(object = "CountMatrix"),
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

# AoristicSum ==================================================================
#' @export
#' @method autoplot AoristicSum
autoplot.AoristicSum <- function(object, ..., facet = TRUE) {
  ## Prepare data
  data <- prepare_aoristic(object)

  ## ggplot
  if (facet) {
    facet <- ggplot2::facet_wrap(
      facets = ggplot2::vars(.data$group),
      nrow = length(unique(data$group)),
      scales = "free_y"
    )
    aes_plot <- ggplot2::aes(x = .data$x, y = .data$y)
  } else {
    facet <- NULL
    aes_plot <- ggplot2::aes(x = .data$x, y = .data$y, fill = .data$group)
  }
  if (anyNA(data$group)) {
    facet <- NULL
    aes_plot <- ggplot2::aes(x = .data$x, y = .data$y)
  }

  ggplot2::ggplot(data = data, mapping = aes_plot) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(name = "Year CE") +
    ggplot2::scale_y_continuous(name = "Aoristic sum") +
    facet
}

#' @export
#' @rdname plot_time
setMethod("autoplot", "AoristicSum", autoplot.AoristicSum)

#' @export
#' @rdname plot_time
#' @aliases plot,AoristicSum,missing-method
setMethod(
  f = "plot",
  signature = c(x = "AoristicSum", y = "missing"),
  definition = function(x, facet = TRUE) {
    gg <- autoplot(object = x, facet = facet) +
      ggplot2::theme_bw()
    print(gg)
    invisible(x)
  }
)

# RateOfChange =================================================================
#' @export
#' @method autoplot RateOfChange
autoplot.RateOfChange <- function(object, ..., facet = TRUE) {
  ## Prepare data
  data <- prepare_roc(object)

  ## ggplot2
  if (facet) {
    facet <- ggplot2::facet_wrap(
      facets = ggplot2::vars(.data$group),
      nrow = length(unique(data$group)),
      scales = "free_y"
    )
    aes_plot <- ggplot2::aes(x = .data$x, y = .data$y)
  } else {
    facet <- NULL
    aes_plot <- ggplot2::aes(x = .data$x, y = .data$y, colour = .data$group)
  }
  if (anyNA(data$group)) {
    facet <- NULL
    aes_plot <- ggplot2::aes(x = .data$x, y = .data$y)
  }

  ggplot2::ggplot(data = data, mapping = aes_plot) +
    ggplot2::geom_hline(yintercept = 0, linetype = 3) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_x_discrete(name = "Year CE") +
    ggplot2::scale_y_continuous(name = "Rate of Change") +
    facet
}

#' @export
#' @rdname plot_time
setMethod("autoplot", "RateOfChange", autoplot.RateOfChange)

#' @export
#' @rdname plot_time
#' @aliases plot,RateOfChange,missing-method
setMethod(
  f = "plot",
  signature = c(x = "RateOfChange", y = "missing"),
  definition = function(x, facet = TRUE) {
    gg <- autoplot(object = x, facet = facet) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
    print(gg)
    invisible(x)
  }
)

# DateMCD ======================================================================
#' @export
#' @method autoplot DateMCD
autoplot.DateMCD <- function(object, ..., select = NULL, decreasing = TRUE,
                             n = 30) {
  boot <- bootstrap(object, probs = c(0.05, 0.95), n = n)
  boot$dates <- get_mcd(object)
  boot$samples <- get_samples(object)

  ## Select data
  index <- select_by_indices(cases = boot$samples, select = select)
  boot <- boot[index, ]

  ## Order data
  data <- boot[order(boot$dates), ]
  lvl <- unique(data$samples)
  lvl <- if (decreasing) rev(lvl) else lvl
  data$samples <- factor(data$samples, levels = lvl)

  aes_point <- ggplot2::aes(x = .data$dates, y = .data$samples)
  aes_seg <- ggplot2::aes(x = .data$Q05, y = .data$samples,
                          xend = .data$Q95, yend = .data$samples)

  ggplot2::ggplot(data = data) +
    ggplot2::geom_point(mapping = aes_point) +
    ggplot2::geom_segment(mapping = aes_seg) +
    ggplot2::scale_x_continuous(name = "Mean Ceramic Date (year CE)") +
    ggplot2::scale_y_discrete(name = "Assemblage")
}

#' @export
#' @rdname plot_time
setMethod("autoplot", "DateMCD", autoplot.DateMCD)

#' @export
#' @rdname plot_time
#' @aliases plot,DateMCD,missing-method
setMethod(
  f = "plot",
  signature = c(x = "DateMCD", y = "missing"),
  definition = function(x, select = NULL, decreasing = TRUE, n = 30) {
    gg <- autoplot(object = x, select = select,
                   decreasing = decreasing, n = n) +
      ggplot2::theme_bw()
    print(gg)
    invisible(x)
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
#' @rdname plot_time
setMethod("autoplot", "IncrementTest", autoplot.IncrementTest)

#' @export
#' @rdname plot_time
#' @aliases plot,IncrementTest,missing-method
setMethod(
  f = "plot",
  signature = c(x = "IncrementTest", y = "missing"),
  definition = function(x, level = 0.95, roll = FALSE, window = 3) {
    gg <- autoplot(object = x, level = level, roll = roll, window = window)
    print(gg)
    invisible(x)
  }
)

# Prepare data =================================================================
## Build a long table for ggplot2
## * Must return a data.frame
## * Must preserve original ordering

prepare_aoristic <- function(object) {
  aoristic <- object
  dates <- get_dates(object)
  groups <- get_groups(object)

  grp <- unique(groups)
  data.frame(
    x = dates,
    y = as.vector(aoristic),
    group = rep(grp, each = nrow(aoristic))
  )
}

prepare_roc <- function(object) {
  rates <- object
  blocks <- get_dates(object)
  blocks <- factor(blocks, levels = unique(blocks))

  m <- dim(rates)[[1]]
  p <- dim(rates)[[2]]
  q <- dim(rates)[[3]]
  n <- m * p

  grp <- dimnames(rates)[[3]]
  if (is.null(grp)) grp <- rep(NA_character_, q)

  data.frame(
    x = rep(rep(blocks, each = m), q),
    y = as.vector(rates),
    group = rep(grp, each = n)
  )
}

prepare_time <- function(object, dates) {
  data <- arkhe::as_long(object, factor = TRUE)

  data$x <- data$dates <- dates
  data$y <- data$value

  ## Remove zeros in case of log scale
  data <- data[data$value > 0, ]

  return(data)
}
