# MEAN CERAMIC DATE
#' @include AllClasses.R AllGenerics.R
NULL

# MCD ==========================================================================
#' @export
#' @rdname mcd
#' @aliases mcd,numeric,numeric-method
setMethod(
  f = "mcd",
  signature = c(object = "numeric", dates = "numeric"),
  definition = function(object, dates, calendar = CE()) {
    object <- matrix(object, nrow = 1)
    methods::callGeneric(object, dates = dates, calendar = calendar)
  }
)

#' @export
#' @rdname mcd
#' @aliases mcd,data.frame,numeric-method
setMethod(
  f = "mcd",
  signature = c(object = "data.frame", dates = "numeric"),
  definition = function(object, dates, calendar = CE()) {
    object <- data.matrix(object)
    methods::callGeneric(object, dates = dates, calendar = calendar)
  }
)

#' @export
#' @rdname mcd
#' @aliases mcd,matrix,numeric-method
setMethod(
  f = "mcd",
  signature = c(object = "matrix", dates = "numeric"),
  definition = function(object, dates, calendar = CE()) {
    ## Validation
    arkhe::assert_length(dates, NCOL(object))

    ## Calculate MCD
    mcd_dates <- apply(X = object, MARGIN = 1, FUN = .mcd, dates = dates)

    ts <- aion::series(
      object,
      time = mcd_dates,
      calendar = calendar
    )
    .MeanDate(ts, dates = aion::fixed(dates, calendar = calendar))
  }
)

.mcd <- function(x, dates) {
  x <- stats::weighted.mean(x = dates, w = x)
  round(x, digits = getOption("kairos.precision"))
}

# Resample =====================================================================
#' @export
#' @rdname resample_mcd
#' @aliases bootstrap,MeanDate-method
setMethod(
  f = "bootstrap",
  signature = c(object = "MeanDate"),
  definition = function(object, n = 1000, f = NULL,
                        calendar = getOption("kairos.calendar")) {

    w <- object
    m <- nrow(w)
    p <- ncol(w)
    seq_col <- seq_len(p)

    dates <- aion::as_year(object@dates, calendar = calendar)
    theta <- function(x, counts, dates) {
      .mcd(counts[x], dates[x])
    }

    results <- vector(mode = "list", length = m)
    for (i in seq_len(m)) {
      results[[i]] <- arkhe::bootstrap(
        object = seq_col,
        do = theta,
        n = n,
        counts = w[i, ],
        dates = dates,
        f = f
      )
    }

    results <- do.call(rbind, results)
    rownames(results) <- rownames(w)
    as.data.frame(results)
  }
)

#' @export
#' @rdname resample_mcd
#' @aliases jackknife,MeanDate-method
setMethod(
  f = "jackknife",
  signature = c(object = "MeanDate"),
  definition = function(object, f = NULL,
                        calendar = getOption("kairos.calendar")) {

    w <- object
    m <- nrow(w)
    p <- ncol(w)

    dates <- aion::as_year(object@dates, calendar = calendar)
    theta <- function(x, counts, dates) {
      .mcd(counts[x], dates[x])
    }

    results <- vector(mode = "list", length = m)
    for (i in seq_len(m)) {
      results[[i]] <- arkhe::jackknife(
        object = seq_len(p),
        do = theta,
        counts = w[i, ],
        dates = dates,
        f = f
      )
    }
    results <- do.call(rbind, results)
    rownames(results) <- rownames(w)
    results
  }
)

#' @export
#' @rdname resample_mcd
#' @aliases simulate,MeanDate-method
setMethod(
  f = "simulate",
  signature = c(object = "MeanDate"),
  definition = function(object, nsim = 1000) {

    results <- apply(
      X = object,
      MARGIN = 1,
      FUN = tabula::resample,
      do = .mcd,
      n = nsim,
      dates = object@dates,
      f = function(x) x
    )

    .SimulationMeanDate(object, replications = t(results))
  }
)

# Plot =========================================================================
#' @export
#' @method plot MeanDate
plot.MeanDate <- function(x, calendar = getOption("kairos.calendar"),
                          decreasing = TRUE,
                          main = NULL, sub = NULL,
                          ann = graphics::par("ann"), axes = TRUE,
                          frame.plot = axes,
                          panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  n <- NROW(x)
  sites <- rownames(x) %||% paste0("S1", n)

  ## Graphical parameters
  col <- list(...)$col %||% graphics::par("col")
  bg <- list(...)$bg %||% graphics::par("bg")
  pch <- list(...)$pch %||% c(16)
  cex <- list(...)$cex %||% graphics::par("cex")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")
  if (length(col) != n) col <- rep(col, length.out = n)
  if (length(bg) != n) bg <- rep(bg, length.out = n)
  if (length(pch) != n) pch <- rep(pch, length.out = n)
  if (length(cex) != n) cex <- rep(cex, length.out = n)
  if (length(lwd) != n) lwd <- rep(lwd, length.out = n)

  ## Save and restore
  mar <- graphics::par("mar")
  mar[2] <- inch2line(sites, cex = cex.axis) + 0.5
  old_par <- graphics::par(mar = mar)
  on.exit(graphics::par(old_par))

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  years <- time(x, calendar = NULL)
  xlim <- range(years)
  ylim <- c(1, n)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Order data
  k <- order(years, decreasing = decreasing)

  ## Plot
  graphics::points(x = years[k], y = seq_len(n), col = col[k],
                   bg = bg[k], pch = pch[k], cex = cex[k], lwd = lwd[k])

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    axis_year(x = years, side = 1, format = TRUE, calendar = calendar)
    graphics::axis(side = 2, at = seq_len(n), labels = sites[k],
                   xpd = NA, cex.axis = cex.axis, las = 1,
                   col.axis = col.axis, font.axis = font.axis)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- format(calendar)
    ylab <- NULL
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }

  invisible(x)
}

#' @export
#' @rdname plot_mcd
#' @aliases plot,MeanDate,missing-method
setMethod("plot", c(x = "MeanDate", y = "missing"), plot.MeanDate)

#' @export
#' @method plot SimulationMeanDate
plot.SimulationMeanDate <- function(x, calendar = getOption("kairos.calendar"),
                                    interval = "student", level = 0.80,
                                    decreasing = TRUE,
                                    main = NULL, sub = NULL,
                                    ann = graphics::par("ann"), axes = TRUE,
                                    frame.plot = axes,
                                    panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  n <- NROW(x)
  sites <- rownames(x) %||% paste0("S1", n)

  ## Compute confidence interval
  fun <- function(x) conf(x, level = level, type = interval)
  inter <- apply(X = x@replications, MARGIN = 1, FUN = fun)

  ## Graphical parameters
  col <- list(...)$col %||% graphics::par("col")
  bg <- list(...)$bg %||% graphics::par("bg")
  pch <- list(...)$pch %||% c(16)
  cex <- list(...)$cex %||% graphics::par("cex")
  lty <- list(...)$lty %||% graphics::par("lty")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")
  if (length(col) != n) col <- rep(col, length.out = n)
  if (length(bg) != n) bg <- rep(bg, length.out = n)
  if (length(pch) != n) pch <- rep(pch, length.out = n)
  if (length(cex) != n) cex <- rep(cex, length.out = n)
  if (length(lty) != n) lty <- rep(lty, length.out = n)
  if (length(lwd) != n) lwd <- rep(lwd, length.out = n)

  ## Save and restore
  mar <- graphics::par("mar")
  mar[2] <- inch2line(sites, cex = cex.axis) + 0.5
  old_par <- graphics::par(mar = mar)
  on.exit(graphics::par(old_par))

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  years <- time(x, calendar = NULL)
  xlim <- range(inter)
  ylim <- c(1, n)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Order data
  k <- order(years, decreasing = decreasing)

  ## Plot
  graphics::segments(x0 = inter[2, k], y0 = seq_len(n),
                     x1 = inter[3, k], y1 = seq_len(n),
                     col = col[k], lty = lty[k], lwd = lwd[k])
  graphics::points(x = years[k], y = seq_len(n), col = col[k],
                   bg = bg[k], pch = pch[k], cex = cex[k], lwd = lwd[k])

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    axis_year(x = years, side = 1, format = TRUE, calendar = calendar)
    graphics::axis(side = 2, at = seq_len(n), labels = sites[k],
                   xpd = NA, cex.axis = cex.axis, las = 1,
                   col.axis = col.axis, font.axis = font.axis)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    ## Caption
    cap <- sprintf("Simulated assemblages, %d replications.", NCOL(x@replications))
    xlab <- format(calendar)
    ylab <- NULL
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }

  invisible(x)
}

#' @export
#' @rdname plot_mcd
#' @aliases plot,SimulationMeanDate,missing-method
setMethod("plot", c(x = "SimulationMeanDate", y = "missing"),
          plot.SimulationMeanDate)

conf <- function(x, type = c("percentiles", "student", "normal", "range"),
                 level = 0.80) {
  type <- match.arg(type, several.ok = FALSE)
  if (type == "range") {
    conf <- range(x, na.rm = FALSE)
  } else if (type == "percentiles") {
    ## Confidence interval as described in Kintigh 1989
    k <- (1 - level) / 2
    conf <- stats::quantile(x, probs = c(k, 1 - k), names = FALSE)
  } else {
    ## Confidence interval
    conf <- arkhe::confidence_mean(x, level = level, type = type)
  }

  result <- c(mean(x), conf)
  names(result) <- c("mean", "lower", "upper")
  result
}
