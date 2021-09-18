# MEAN CERAMIC DATE
#' @include AllClasses.R AllGenerics.R
NULL

# MCD ==========================================================================
#' @export
#' @rdname mcd
#' @aliases mcd,CountMatrix,numeric-method
setMethod(
  f = "mcd",
  signature = signature(object = "CountMatrix", dates = "numeric"),
  definition = function(object, dates) {
    ## Validation
    arkhe::assert_length(dates, ncol(object))

    ## Calculate MCD
    mcd_dates <- apply(
      X = object,
      MARGIN = 1,
      FUN = mean_date,
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
#' @rdname mcd
#' @aliases bootstrap,MeanDate-method
setMethod(
  f = "bootstrap",
  signature = signature(object = "MeanDate"),
  definition = function(object, level = 0.95, type = c("student", "normal"),
                        probs = c(0.25, 0.50, 0.75), n = 1000) {

    theta <- function(x, do, n, dates, level, type, probs) {
      boot <- dimensio::bootstrap(x, do, n, dates)
      dimensio::summary(boot, level = level, type = type, probs = probs)
    }
    results <- apply(
      X = get_weights(object),
      MARGIN = 1,
      FUN = theta,
      do = mean_date,
      n = n,
      dates = object@types,
      level = level,
      type = type,
      probs = probs
    )
    as.data.frame(t(results))
  }
)

#' @export
#' @rdname mcd
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
      mean_date(counts[x], dates[x])
    }

    results <- vector(mode = "list", length = m)
    for (i in seq_len(m)) {
      jack <- dimensio::jackknife(
        object = seq_len(p),
        do = theta,
        counts = w[i, ],
        dates = dates
      )
      results[[i]] <- dimensio::summary(jack)
    }
    results <- do.call(rbind, results)
    rownames(results) <- rownames(w)
    as.data.frame(results)
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
mean_date <- function(counts, dates, na.rm = FALSE) {
  stats::weighted.mean(x = dates, w = counts, na.rm = na.rm)
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
setMethod("autoplot", "MeanDate", autoplot.MeanDate)

#' @export
#' @rdname plot_mcd
#' @aliases plot,MeanDate,missing-method
setMethod(
  f = "plot",
  signature = c(x = "MeanDate", y = "missing"),
  definition = function(x, select = NULL, decreasing = TRUE) {
    gg <- autoplot(object = x, select = select, decreasing = decreasing) +
      ggplot2::theme_bw()
    print(gg)
    invisible(x)
  }
)
