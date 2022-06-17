# MEAN CERAMIC DATE
#' @include AllClasses.R AllGenerics.R
NULL

# MCD ==========================================================================
#' @export
#' @rdname mcd
#' @aliases mcd,numeric,numeric-method
setMethod(
  f = "mcd",
  signature = signature(object = "numeric", dates = "numeric"),
  definition = function(object, dates) {
    ## Validation
    arkhe::assert_length(dates, length(object))
    # arkhe::assert_count(object)

    x <- stats::weighted.mean(x = dates, w = object)
    round(x, digits = getOption("kairos.precision"))
  }
)

#' @export
#' @rdname mcd
#' @aliases mcd,data.frame,numeric-method
setMethod(
  f = "mcd",
  signature = signature(object = "data.frame", dates = "numeric"),
  definition = function(object, dates) {
    object <- data.matrix(object)
    methods::callGeneric(object, dates = dates)
  }
)

#' @export
#' @rdname mcd
#' @aliases mcd,matrix,numeric-method
setMethod(
  f = "mcd",
  signature = signature(object = "matrix", dates = "numeric"),
  definition = function(object, dates) {
    ## Calculate MCD
    mcd_dates <- apply(
      X = object,
      MARGIN = 1,
      FUN = mcd,
      dates = dates
    )

    names(mcd_dates) <- rownames(object)
    .MeanDate(
      mcd_dates,
      weights = object,
      types = dates
    )
  }
)

# Resample =====================================================================
#' @export
#' @rdname resample
#' @aliases bootstrap,MeanDate-method
setMethod(
  f = "bootstrap",
  signature = signature(object = "MeanDate"),
  definition = function(object, n = 1000, f = NULL) {

    w <- get_weights(object)
    m <- nrow(w)

    dates <- object@types
    theta <- function(x, counts, dates) {
      mcd(counts[x], dates[x])
    }

    results <- vector(mode = "list", length = m)
    for (i in seq_len(m)) {
      b <- boot::boot(w[i, ], statistic = theta, R = n, dates = dates)
      if (is.null(f)) {
        results[[i]] <- summary_bootstrap(b$t, b$t0)
      } else {
        results[[i]] <- f(as.numeric(b$t))
      }
    }
    results <- do.call(rbind, results)
    rownames(results) <- rownames(w)
    as.data.frame(results)
  }
)

summary_bootstrap <- function(x, hat) {
  n <- length(x)
  boot_mean <- mean(x)
  boot_bias <- boot_mean - hat
  boot_error <- stats::sd(x)

  results <- c(hat, boot_mean, boot_bias, boot_error)
  names(results) <- c("original", "mean", "bias", "error")
  results
}

#' @export
#' @rdname resample
#' @aliases jackknife,MeanDate-method
setMethod(
  f = "jackknife",
  signature = signature(object = "MeanDate"),
  definition = function(object) {

    w <- get_weights(object)
    m <- nrow(w)
    p <- ncol(w)

    dates <- object@types
    theta <- function(x, counts, dates) {
      mcd(counts[x], dates[x])
    }

    results <- vector(mode = "list", length = m)
    for (i in seq_len(m)) {
      results[[i]] <- arkhe::jackknife(
        object = seq_len(p),
        do = theta,
        counts = w[i, ],
        dates = dates
      )
    }
    results <- do.call(rbind, results)
    rownames(results) <- rownames(w)
    results
  }
)

#' @export
#' @rdname resample
#' @aliases simulate,MeanDate-method
setMethod(
  f = "simulate",
  signature = signature(object = "MeanDate"),
  definition = function(object, n = 1000,
                        interval = c("student", "normal", "percentiles"),
                        level = 0.80) {

    fun <- function(x) conf(x, level = level, type = interval)
    results <- apply(
      X = get_weights(object),
      MARGIN = 1,
      FUN = resample,
      do = mcd,
      n = n,
      dates = object@types,
      f = fun
    )

    sim <- .SimulationMeanDate(
      object,
      simulation = t(results),
      level = level,
      replications = as.integer(n)
    )
    names(sim) <- names(object)
    sim
  }
)

conf <- function(x, type = c("percentiles", "student", "normal"),
                 level = 0.80) {
  type <- match.arg(type, several.ok = FALSE)
  if (type == "percentiles") {
    ## Confidence interval as described in Kintigh 1989
    k <- (1 - level) / 2
    conf <- stats::quantile(x, probs = c(k, 1 - k), names = FALSE)
  } else {
    ## Confidence interval
    conf <- arkhe::confidence(x, level = level, type = type)
  }

  result <- c(mean(x), conf)
  names(result) <- c("mean", "lower", "upper")
  result
}

# Plot =========================================================================
#' @export
#' @method autoplot MeanDate
autoplot.MeanDate <- function(object, ..., select = NULL, decreasing = TRUE) {
  ## Select data
  data <- as.data.frame(object)
  index <- select_by_indices(cases = data$names, select = select)
  data <- data[index, ]

  ## Order data
  data <- data[order(data$dates), ]
  lvl <- unique(data$names)
  lvl <- if (decreasing) rev(lvl) else lvl
  data$names <- factor(data$names, levels = lvl)

  ## ggplot2
  aes_point <- ggplot2::aes(x = .data$dates, y = .data$names)

  ggplot2::ggplot(data = data) +
    ggplot2::geom_point(mapping = aes_point) +
    ggplot2::scale_x_continuous(name = "Mean Date (year CE)") +
    ggplot2::scale_y_discrete(name = "Assemblage")
}

#' @export
#' @rdname plot_mcd
#' @aliases autoplot,MeanDate-method
setMethod("autoplot", "MeanDate", autoplot.MeanDate)

#' @export
#' @method plot MeanDate
plot.MeanDate <- function(x, select = NULL, decreasing = TRUE, ...) {
  gg <- autoplot(object = x, select = select, decreasing = decreasing) +
    ggplot2::theme_bw()
  print(gg)
  invisible(x)
}

#' @export
#' @rdname plot_mcd
#' @aliases plot,MeanDate,missing-method
setMethod("plot", c(x = "MeanDate", y = "missing"), plot.MeanDate)

#' @export
#' @method autoplot SimulationMeanDate
autoplot.SimulationMeanDate <- function(object, ..., select = NULL,
                                        decreasing = TRUE) {
  ## Select data
  data <- as.data.frame(object)
  index <- select_by_indices(cases = data$names, select = select)
  data <- data[index, ]

  ## Order data
  data <- data[order(data$dates), ]
  lvl <- unique(data$names)
  lvl <- if (decreasing) rev(lvl) else lvl
  data$names <- factor(data$names, levels = lvl)

  ## ggplot2
  aes_point <- ggplot2::aes(x = .data$mean, y = .data$names)
  aes_segm <- ggplot2::aes(x = .data$lower, xend = .data$upper,
                           y = .data$names, yend = .data$names)

  ggplot2::ggplot(data = data) +
    ggplot2::geom_segment(mapping = aes_segm) +
    ggplot2::geom_point(mapping = aes_point) +
    ggplot2::scale_x_continuous(name = "Mean Date (year CE)") +
    ggplot2::scale_y_discrete(name = "Assemblage")
}

#' @export
#' @rdname plot_mcd
#' @aliases autoplot,SimulationMeanDate-method
setMethod("autoplot", "SimulationMeanDate", autoplot.SimulationMeanDate)

#' @export
#' @method plot SimulationMeanDate
plot.SimulationMeanDate <- function(x, select = NULL, decreasing = TRUE, ...) {
  cap <- sprintf("Simulated assemblages, %d replications.", x@replications)
  gg <- autoplot(object = x, select = select, decreasing = decreasing) +
    ggplot2::labs(caption = cap) +
    ggplot2::theme_bw()
  print(gg)
  invisible(x)
}

#' @export
#' @rdname plot_mcd
#' @aliases plot,SimulationMeanDate,missing-method
setMethod("plot", c(x = "SimulationMeanDate", y = "missing"),
          plot.SimulationMeanDate)
