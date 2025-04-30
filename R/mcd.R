# MEAN CERAMIC DATE
#' @include AllGenerics.R
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

    if (is.null(calendar)) {
      dates <- aion::as_fixed(dates) # Assume rata die
    } else {
      dates <- aion::fixed(dates, calendar = calendar)
    }

    ## Calculate MCD
    mcd_dates <- apply(X = object, MARGIN = 1, FUN = .mcd, dates = dates)
    mcd_dates <- aion::as_fixed(mcd_dates)

    ts <- aion::series(object, time = mcd_dates)
    rownames(ts) <- rownames(object)
    .MeanDate(ts, dates = dates)
  }
)

.mcd <- function(x, dates) {
  stats::weighted.mean(x = dates, w = x)
}

# Plot =========================================================================
#' @export
#' @method plot MeanDate
plot.MeanDate <- function(x, calendar = get_calendar(),
                          decreasing = TRUE,
                          main = NULL, sub = NULL,
                          ann = graphics::par("ann"), axes = TRUE,
                          frame.plot = axes,
                          panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  n <- NROW(x)
  n_seq <- seq_len(n)
  sites <- rownames(x) %||% paste0("S", n_seq)

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
  years <- time(x, calendar = calendar)
  xlim <- range(years)
  ylim <- c(1, n)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Order data
  k <- order(years, decreasing = decreasing)

  ## Plot
  graphics::points(x = years[k], y = n_seq, col = col[k],
                   bg = bg[k], pch = pch[k], cex = cex[k], lwd = lwd[k])

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    aion::year_axis(side = 1, format = TRUE, calendar = calendar,
                    current_calendar = calendar, cex.axis = cex.axis,
                    col.axis = col.axis, font.axis = font.axis)
    graphics::axis(side = 2, at = n_seq, labels = sites[k],
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
#' @rdname plot.MeanDate
#' @aliases plot,MeanDate,missing-method
setMethod("plot", c(x = "MeanDate", y = "missing"), plot.MeanDate)

#' @export
#' @method plot SimulationMeanDate
plot.SimulationMeanDate <- function(x, calendar = get_calendar(),
                                    interval = "student", level = 0.80,
                                    decreasing = TRUE,
                                    main = NULL, sub = NULL,
                                    ann = graphics::par("ann"), axes = TRUE,
                                    frame.plot = axes,
                                    panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  n <- NROW(x)
  n_seq <- seq_len(n)
  sites <- rownames(x) %||% paste0("S1", n_seq)

  ## Compute confidence interval
  fun <- function(x) conf(x, level = level, type = interval)
  inter <- apply(X = x@replications, MARGIN = 1, FUN = fun)
  inter <- apply(X = inter, MARGIN = 1, FUN = as_year, calendar = calendar)

  ## Graphical parameters
  col <- list(...)$col %||% graphics::par("col")
  bg <- list(...)$bg %||% graphics::par("bg")
  pch <- list(...)$pch %||% c(16)
  cex <- list(...)$cex %||% graphics::par("cex")
  lty <- list(...)$lty %||% graphics::par("lty")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  if (length(col) != n) col <- rep(col, length.out = n)
  if (length(bg) != n) bg <- rep(bg, length.out = n)
  if (length(pch) != n) pch <- rep(pch, length.out = n)
  if (length(cex) != n) cex <- rep(cex, length.out = n)
  if (length(lty) != n) lty <- rep(lty, length.out = n)
  if (length(lwd) != n) lwd <- rep(lwd, length.out = n)
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")

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
  years <- time(x, calendar = calendar)
  xlim <- range(inter)
  ylim <- c(1, n)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Order data
  k <- order(years, decreasing = decreasing)

  ## Plot
  graphics::segments(x0 = inter[k, 2], y0 = n_seq,
                     x1 = inter[k, 3], y1 = n_seq,
                     col = col[k], lty = lty[k], lwd = lwd[k])
  graphics::points(x = years[k], y = n_seq, col = col[k],
                   bg = bg[k], pch = pch[k], cex = cex[k], lwd = lwd[k])

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    aion::year_axis(side = 1, format = TRUE, calendar = calendar,
                    current_calendar = calendar, cex.axis = cex.axis,
                    col.axis = col.axis, font.axis = font.axis)
    graphics::axis(side = 2, at = n_seq, labels = sites[k],
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
    cap <- sprintf(tr_("Simulated assemblages, %d replications."), NCOL(x@replications))
    xlab <- format(calendar)
    ylab <- NULL
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }

  invisible(x)
}

#' @export
#' @rdname plot.MeanDate
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
