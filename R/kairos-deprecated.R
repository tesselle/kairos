#' Deprecated Functions in kairos
#'
#' These functions still work but will be removed (defunct) in the next version.
#' @name kairos-deprecated
#' @keywords internal
NULL

.SimulationMeanDate <- setClass(
  Class = "SimulationMeanDate",
  slots = c(
    replications = "matrix",
    seed = "numeric"
  ),
  contains = "MeanDate"
)

#' @export
#' @method simulate MeanDate
simulate.MeanDate <- function(object, nsim = 1000, seed = NULL, ...) {
  .Deprecated(new = "bootstrap()", old = "simulate()")

  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    stats::runif(1)
  if (is.null(seed))
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  results <- apply(
    X = object[, , 1, drop = TRUE],
    MARGIN = 1,
    FUN = resample,
    do = .mcd,
    n = nsim,
    dates = object@dates
  )

  .SimulationMeanDate(object, replications = t(results), seed = RNGstate)
}

#' @export
#' @rdname kairos-deprecated
#' @aliases simulate,MeanDate-method
setMethod("simulate", c(object = "MeanDate"), simulate.MeanDate)

resample <- function(x, do, n, size = sum(x), ...) {
  prob <- x / sum(x)
  replicates <- stats::rmultinom(n, size = size, prob = prob)
  apply(X = replicates, MARGIN = 2, FUN = do, ...)
}

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
#' @rdname kairos-deprecated
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

#' @export
#' @rdname kairos-deprecated
setGeneric(
  name = "get_order",
  def = function(x, ...) standardGeneric("get_order")
)

#' @export
#' @rdname kairos-deprecated
setMethod(
  f = "get_order",
  signature = c("PermutationOrder"),
  definition = function(x, margin = c(1, 2)) {
    .Deprecated(new = "order_rows()/order_columns()", old = "get_order()")
    o <- list(rows = integer(0), columns = integer(0))
    o$rows <- if (1 %in% margin) x@rows_order else NULL
    o$columns <- if (2 %in% margin) x@columns_order else NULL
    if (length(o) == 1) unname(unlist(o)) else o
  }
)

.RefinePermutationOrder <- setClass(
  Class = "RefinePermutationOrder",
  slots = c(
    length = "numeric",
    cutoff = "numeric",
    keep = "integer",
    margin = "integer"
  ),
  contains = "AveragePermutationOrder"
)

#' @rdname kairos-deprecated
#' @aliases seriate_refine-method
setGeneric(
  name = "seriate_refine",
  def = function(object, ...) standardGeneric("seriate_refine")
)

#' @export
#' @rdname kairos-deprecated
#' @aliases seriate_refine,AveragePermutationOrder-method
setMethod(
  f = "seriate_refine",
  signature = c(object = "AveragePermutationOrder"),
  definition = function(object, cutoff, margin = 1, axes = 1, n = 30, ...) {
    ## Partial bootstrap CA
    ## /!\ Be careful: AveragePermutationOrder inherits from CA
    object <- dimensio::bootstrap(object, n = n)
    methods::callGeneric(object, cutoff = cutoff, margin = margin, axes = axes, ...)
  }
)

#' @export
#' @rdname kairos-deprecated
#' @aliases seriate_refine,BootstrapCA-method
setMethod(
  f = "seriate_refine",
  signature = c(object = "BootstrapCA"),
  definition = function(object, cutoff, margin = 1, axes = 1, ...) {
    .Deprecated(new = "refine", old = "seriate_refine()")
    ## Validation
    arkhe::assert_function(cutoff)

    ca_rep <- dimensio::get_replications(object, margin = margin)

    ## Compute convex hull
    hull <- apply(
      X = ca_rep,
      MARGIN = 1,
      FUN = function(x, axes) compute_chull(t(x), c(1, 2)),
      axes = axes
    )

    ## Get convex hull maximal dimension length for each sample
    len <- vapply(
      X = hull,
      FUN = function(x) max(stats::dist(x, method = "euclidean")),
      FUN.VALUE = double(1)
    )

    ## Get cutoff values
    limit <- cutoff(len)

    ## Samples to be excluded
    sup <- which(len >= limit)

    ## Seriation
    ser <- seriate_average(dimensio::get_data(object), margin = c(1, 2),
                           axes = axes, sup_row = sup, ...)

    .RefinePermutationOrder(
      ser,
      length = len,
      cutoff = limit,
      keep = which(len < limit),
      margin = as.integer(margin)
    )
  }
)

#' @export
#' @method hist RefinePermutationOrder
hist.RefinePermutationOrder <- function(x, ...) {
  ## Get data
  cutoff <- x@cutoff

  graphics::hist(
    x = x@length,
    xlab = tr_("Maximum length"),
    main = NULL,
    las = 1,
    ...
  )
  graphics::abline(v = cutoff, col = "red")
  graphics::axis(side = 3, at = cutoff, labels = round(cutoff, 2), tick = FALSE,
                 col = "red", col.ticks = "red", col.axis = "red")

  invisible(x)
}

#' @export
#' @rdname kairos-deprecated
# @describeIn seriate_refine Compute and plot a histogram of convex hull
#  maximum dimension length.
#' @aliases hist,RefinePermutationOrder-method
setMethod("hist", "RefinePermutationOrder", hist.RefinePermutationOrder)
