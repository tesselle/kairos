# AORISTIC ANALYSIS
#' @include AllClasses.R AllGenerics.R
NULL

# Aoristic sum =================================================================
#' @export
#' @rdname aoristic
#' @aliases aoristic,numeric,numeric-method
setMethod(
  f = "aoristic",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, step = 1, start = min(x), end = max(y),
                        calendar = CE(), weight = TRUE, groups = NULL) {
    ## Validation
    n <- length(x)
    arkhe::assert_length(y, n)
    if (is.null(groups)) groups <- rep(NA, n)
    groups <- as.character(groups)
    arkhe::assert_length(groups, n)

    ## Grouping
    lvl <- unique(groups)
    grp <- factor(groups, levels = lvl, exclude = NULL)
    spl <- split(seq_along(x), f = grp)
    m <- length(spl)

    ## Convert to rata die
    ## NB: argument are not evaluated until they are used.
    ## ('start' and 'end' must go first)
    start <- aion::fixed(start, calendar = calendar)
    end <- aion::fixed(end, calendar = calendar)
    step <- aion::fixed(step, calendar = calendar)
    x <- aion::fixed(x, calendar = calendar)
    y <- aion::fixed(y, calendar = calendar)

    ## Set up breaks
    breaks <- seq(from = start, to = end, by = step)
    block_start <- utils::head(breaks, -1)
    block_end <- utils::tail(breaks, -1)
    block_mid <- block_start + step / 2

    ## Number of time blocks
    n_blocks <- length(breaks) - 1
    ao_probs <- array(data = 0, dim = c(n, n_blocks, m))

    ## Aoristic probability
    span <- abs(y - x)
    for (k in seq_len(m)) {
      s <- spl[[k]]
      for (j in seq_len(n_blocks)) {
        a <- block_start[[j]]
        b <- block_end[[j]]

        max_start <- pmax(x[s], a)
        min_end <- pmin(y[s], b)
        overlap <- (min_end - max_start) / span[s]
        overlap[overlap <= 0] <- 0
        if (!weight) overlap[overlap > 0] <- 1
        ao_probs[s, j, k] <- overlap
      }
    }

    ## Aoristic sum
    ao_sum <- apply(X = ao_probs, MARGIN = c(2, 3), FUN = sum)

    if (!anyNA(groups)) {
      dimnames(ao_probs)[[3]] <- colnames(ao_sum) <- lvl
    }

    ts <- aion::series(
      ao_sum,
      time = block_mid
    )
    .AoristicSum(
      ts,
      weights = span,
      groups = groups,
      breaks = breaks,
      p = ao_probs
    )
  }
)

#' @export
#' @rdname aoristic
#' @aliases aoristic,ANY,missing-method
setMethod(
  f = "aoristic",
  signature = c(x = "ANY", y = "missing"),
  definition = function(x, step = 1, start = NULL, end = NULL,
                        calendar = CE(), weight = TRUE, groups = NULL) {
    xy <- grDevices::xy.coords(x)
    g <- groups
    if (!is.null(g) && length(g) == 1) {
      g <- x[[g]]
      if (is.null(g)) {
        msg <- sprintf("%s is a list, but does not have component %s.",
                       sQuote("x"), sQuote(groups))
        stop(msg, call. = FALSE)
      }
    }

    ## Start/end
    if (is.null(start)) start <- min(xy$x, na.rm = TRUE)
    if (is.null(end)) end <- max(xy$y, na.rm = TRUE)

    methods::callGeneric(x = xy$x, y = xy$y, step = step, start = start,
                         end = end, calendar = calendar, weight = weight,
                         groups = g)
  }
)

# Rate of change ===============================================================
#' @export
#' @rdname roc
#' @aliases roc,AoristicSum-method
setMethod(
  f = "roc",
  signature = c(object = "AoristicSum"),
  definition = function(object, n = 100) {
    ## Get time span of the blocks
    step <- min(diff(object@breaks))
    end <- utils::tail(object@breaks, -1)
    mid <- utils::head(end, -1)

    ## Get aoristic weights
    w <- object@p

    n <- as.integer(n)
    m <- dim(w)[[1]]
    p <- dim(w)[[2]]
    q <- dim(w)[[3]]

    ## Monte Carlo simulation
    roc <- array(data = 0, dim = c(n, p-1, q))
    for (j in seq_len(q)) {
      sim <- array(data = 0, dim = c(m, p, n))
      for (i in seq_len(m)) {
        probs <- w[i, , j]
        if (sum(probs) == 0) next
        counts <- replicate(n, {
          tmp <- numeric(p)
          k <- sample(seq_len(p), size = 1, prob = probs)
          tmp[k] <- 1
          tmp
        })
        sim[i, , ] <- counts
      }
      total <- apply(X = sim, MARGIN = c(2, 3), FUN = sum)
      roc[, , j] <- t(apply(X = total, MARGIN = 2, FUN = diff, lag = 1))
    }

    dimnames(roc)[[3]] <- dimnames(w)[[3]]

    .RateOfChange(
      roc / step,
      replicates = n,
      breaks = mid,
      groups = unique(object@groups)
    )
  }
)

# Plot =========================================================================
## AoristicSum -----------------------------------------------------------------
#' @export
#' @method plot AoristicSum
plot.AoristicSum <- function(x, calendar = getOption("kairos.calendar"),
                             type = c("bar", "density"),
                             panel = graphics::lines, flip = FALSE, ncol = NULL,
                             main = NULL, sub = NULL,
                             ann = graphics::par("ann"), axes = TRUE,
                             frame.plot = axes,
                             panel.first = NULL, panel.last = NULL, ...) {
  ## Validation
  type <- match.arg(type, several.ok = FALSE)
  n <- NCOL(x)
  n_seq <- seq_len(n)
  if (is.null(ncol)) ncol <- if (n > 4) 2 else 1
  n_row <- ceiling(n / ncol)
  ylabs <- colnames(x) %||%  c("Aoristic Sum")

  ## Graphical parameters
  ## Save and restore
  old_par <- graphics::par(
    mar = c(0, 5.1, 0, if (flip) 5.1 else 2.1),
    oma = c(6, 0, 5, 0),
    mfcol = c(n_row, ncol)
  )
  on.exit(graphics::par(old_par))

  cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")
  col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
  font.lab <- list(...)$font.lab %||% graphics::par("font.lab")
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")
  cex.main <- list(...)$cex.main %||% graphics::par("cex.main")
  font.main <- list(...)$font.main %||% graphics::par("font.main")
  col.main <- list(...)$col.main %||% graphics::par("col.main")
  col <- list(...)$col %||% c("grey")
  if (length(col) != n) col <- rep(col, n)

  years <- time(x, calendar = NULL)
  start <- utils::head(x@breaks, -1)
  end <- utils::tail(x@breaks, -1)
  step <- min(diff(x@breaks))
  xlim <- range(x@breaks)
  for (i in n_seq) {
    xi <- x[, i, drop = TRUE]
    if (type == "density") {
      s <- sample(x = years, size = 10^5, replace = TRUE, prob = xi)
      d <- stats::density(x = s, bw = step, ...)
      xi <- d$y
    }

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    ylim <- range(xi)
    graphics::plot.window(xlim = xlim, ylim = ylim)

    ## Evaluate pre-plot expressions
    panel.first

    ## Plot
    if (type == "bar") {
      graphics::rect(xleft = start, xright = end,
                     ybottom = 0, ytop = xi, col = col[i])
    }
    if (type == "density") {
      graphics::polygon(x = c(d$x, rev(d$x)), y = c(xi, numeric(length(xi))),
                        col = col[i], border = NA)
      graphics::lines(x = d$x, y = xi)
    }

    ## Evaluate post-plot and pre-axis expressions
    panel.last

    ## Construct Axis
    do_x <- i %% n_row == 0 || i == n
    y_side <- if (i %% 2 || !flip) 2 else 4
    if (axes) {
      if (do_x) {
        axis_year(x = years, side = 1, format = TRUE, calendar = calendar,
                  xpd = NA, cex.axis = cex.axis,
                  col.axis = col.axis, font.axis = font.axis)
      }
      graphics::axis(side = y_side, xpd = NA, cex.axis = cex.axis,
                     col.axis = col.axis, font.axis = font.axis, las = 1)
    }

    ## Plot frame
    if (frame.plot) {
      graphics::box()
    }

    ## Add annotation
    if (ann) {
      if (do_x) {
        xlab <- format(calendar)
        graphics::mtext(xlab, side = 1, line = 3, cex = cex.lab, col = col.lab,
                        font = font.lab)
      }
      graphics::mtext(ylabs[[i]], side = y_side, line = 3, cex = cex.lab,
                      col = col.lab, font = font.lab)
    }
  }

  ## Add annotation
  if (ann) {
    graphics::par(mfcol = c(1, 1))
    graphics::mtext(main, side = 3, line = 4, cex = cex.main, font = font.main,
                    col = col.main)
  }

  invisible(x)
}

#' @export
#' @rdname plot_aoristic
#' @aliases plot,AoristicSum,missing-method
setMethod("plot", c(x = "AoristicSum", y = "missing"), plot.AoristicSum)

#' @export
#' @method image AoristicSum
image.AoristicSum <- function(x, calendar = getOption("kairos.calendar"), ...) {
  methods::callNextMethod()
  invisible(x)
}

#' @export
#' @rdname plot_aoristic
#' @aliases image,AoristicSum-method
setMethod("image", c(x = "AoristicSum"), image.AoristicSum)

## RateOfChange ----------------------------------------------------------------
#' @export
#' @method plot RateOfChange
plot.RateOfChange <- function(x, calendar = getOption("kairos.calendar"),
                              level = 0.95,
                              panel = graphics::lines, flip = FALSE,
                              ncol = NULL, main = NULL, sub = NULL,
                              ann = graphics::par("ann"), axes = TRUE,
                              frame.plot = axes,
                              panel.first = NULL, panel.last = NULL, ...) {
  ## Validation
  n <- dim(x)[[3L]]
  n_seq <- seq_len(n)
  if (is.null(ncol)) ncol <- if (n > 4) 2 else 1
  n_row <- ceiling(n / ncol)
  ylabs <- dimnames(x)[[3L]] %||% c("Rate of Change")

  ## Compute mean and confidence interval
  ci <- apply(
    X = x,
    MARGIN = c(2, 3),
    FUN = function(x, level) {
      c(mean = mean(x), arkhe::confidence_mean(x, level))
    },
    level = level
  )

  ## Graphical parameters
  ## Save and restore
  old_par <- graphics::par(
    mar = c(0, 5.1, 0, if (flip) 5.1 else 2.1),
    oma = c(6, 0, 5, 0),
    mfcol = c(n_row, ncol)
  )
  on.exit(graphics::par(old_par))

  cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")
  col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
  font.lab <- list(...)$font.lab %||% graphics::par("font.lab")
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")
  cex.main <- list(...)$cex.main %||% graphics::par("cex.main")
  font.main <- list(...)$font.main %||% graphics::par("font.main")
  col.main <- list(...)$col.main %||% graphics::par("col.main")
  col <- list(...)$col %||% c("grey")
  if (length(col) != n) col <- rep(col, n)

  years <- x@breaks
  xlim <- range(years)
  for (i in n_seq) {
    xi <- ci[, , i, drop = TRUE]

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    ylim <- range(xi)
    graphics::plot.window(xlim = xlim, ylim = ylim)

    ## Evaluate pre-plot expressions
    panel.first

    ## Plot
    graphics::abline(h = 0, lty = "dotted", lwd = 1)
    graphics::polygon(x = c(years, rev(years)),
                      y = c(xi[2, ], rev(xi[3, ])),
                      col = col[i], border = NA)
    graphics::lines(x = years, y = xi[1, ])
    graphics::points(x = years, y = xi[1, ], pch = 16)

    ## Evaluate post-plot and pre-axis expressions
    panel.last

    ## Construct Axis
    do_x <- i %% n_row == 0 || i == n
    y_side <- if (i %% 2 || !flip) 2 else 4
    if (axes) {
      if (do_x) {
        axis_year(x = years, side = 1, format = TRUE, calendar = calendar,
                  xpd = NA, cex.axis = cex.axis,
                  col.axis = col.axis, font.axis = font.axis)
      }
      graphics::axis(side = y_side, xpd = NA, cex.axis = cex.axis,
                     col.axis = col.axis, font.axis = font.axis, las = 1)
    }

    ## Plot frame
    if (frame.plot) {
      graphics::box()
    }

    ## Add annotation
    if (ann) {
      if (do_x) {
        xlab <- format(calendar)
        graphics::mtext(xlab, side = 1, line = 3, cex = cex.lab, col = col.lab,
                        font = font.lab)
      }
      graphics::mtext(ylabs[[i]], side = y_side, line = 4, cex = cex.lab,
                      col = col.lab, font = font.lab)
    }
  }

  ## Add annotation
  if (ann) {
    graphics::par(mfcol = c(1, 1))
    graphics::mtext(main, side = 3, line = 3, cex = cex.main, font = font.main,
                    col = col.main)
  }

  invisible(x)
}

#' @export
#' @rdname plot_aoristic
#' @aliases plot,RateOfChange,missing-method
setMethod("plot", c(x = "RateOfChange", y = "missing"), plot.RateOfChange)
