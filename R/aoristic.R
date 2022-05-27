# AORISTIC ANALYSIS
#' @include AllClasses.R AllGenerics.R
NULL

# Aoristic sum =================================================================
#' @export
#' @rdname aoristic
#' @aliases aoristic,numeric,numeric-method
setMethod(
  f = "aoristic",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, step = 1, start = min(x, na.rm = TRUE),
                        stop = max(y, na.rm = TRUE), weight = TRUE,
                        groups = NULL) {
    ## Validation
    n <- length(x)
    arkhe::assert_length(y, n)
    if (!is.null(groups)) {
      arkhe::assert_length(groups, n)
      groups <- as.character(groups)
    } else {
      groups <- rep(NA_character_, n)
    }

    ## Grouping
    lvl <- unique(groups)
    grp <- factor(groups, levels = lvl, exclude = NULL)
    spl <- split(seq_along(x), f = grp)
    m <- length(spl)

    ## Set up breaks
    breaks <- seq(from = start, to = stop, by = step)
    block_start <- utils::head(breaks, -1)
    block_end <- utils::tail(breaks, -1)

    blocks <- paste(block_start, block_end, sep = "_")
    ao_probs <- array(data = 0, dim = c(n, length(blocks), m))
    dimnames(ao_probs) <- list(NULL, blocks)

    ## Aoristic probability
    span <- abs(y - x)
    for (k in seq_len(m)) {
      s <- spl[[k]]
      for (j in seq_along(blocks)) {
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
    ao_sum <- as.matrix(ao_sum)

    if (!anyNA(groups)) {
      dimnames(ao_probs)[[3]] <- colnames(ao_sum) <- lvl
    }

    .AoristicSum(
      ao_sum,
      from = x,
      to = y,
      step = step,
      weights = span,
      groups = groups,
      breaks = breaks,
      blocks = blocks,
      p = ao_probs
    )
  }
)

#' @export
#' @rdname aoristic
#' @aliases aoristic,list,missing-method
setMethod(
  f = "aoristic",
  signature = signature(x = "list", y = "missing"),
  definition = function(x, step = 1, start = NULL, stop = NULL, weight = TRUE,
                        groups = NULL) {
    ## Validation
    k <- match(c("from", "to"), names(x))
    if (anyNA(k)) {
      msg <- sprintf("%s is a list, but does not have components %s and %s.",
                     sQuote("x"), sQuote("from"), sQuote("to"))
      stop(msg, call. = FALSE)
    }
    g <- groups
    if (!is.null(g) && length(g) == 1) {
      g <- x[[g]]
      if (is.null(g)) {
        msg <- sprintf("%s is a list, but does not have component %s.",
                       sQuote("x"), sQuote(groups))
        stop(msg, call. = FALSE)
      }
    }

    ## Start/stop
    if (is.null(start)) start <- min(x$from, na.rm = TRUE)
    if (is.null(stop)) stop <- max(x$to, na.rm = TRUE)

    methods::callGeneric(x = x$from, y = x$to, step = step, start = start,
                         stop = stop, weight = weight, groups = g)
  }
)

# Rate of change ===============================================================
#' @export
#' @rdname roc
#' @aliases roc,AoristicSum-method
setMethod(
  f = "roc",
  signature = signature(object = "AoristicSum"),
  definition = function(object, n = 100) {
    ## Get time span of the blocks
    step <- object@step
    blocks <- get_dates(object)
    mid <- utils::head(blocks$end, -1)

    ## Get aoristic weights
    w <- get_weights(object)

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
#' @method autoplot AoristicSum
autoplot.AoristicSum <- function(object, ..., facet = TRUE) {
  ## Prepare data
  data <- prepare_aoristic(object)
  bin_width <- object@step * 0.9

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
    ggplot2::geom_col(position = "dodge", width = bin_width) +
    ggplot2::scale_x_continuous(name = "Year CE") +
    ggplot2::scale_y_continuous(name = "Aoristic sum") +
    facet
}

#' @export
#' @rdname plot_aoristic
#' @aliases autoplot,AoristicSum-method
setMethod("autoplot", "AoristicSum", autoplot.AoristicSum)

#' @export
#' @method plot AoristicSum
plot.AoristicSum <- function(x, facet = TRUE, ...) {
  gg <- autoplot(object = x, facet = facet) +
    ggplot2::theme_bw()
  print(gg)
  invisible(x)
}

#' @export
#' @rdname plot_aoristic
#' @aliases plot,AoristicSum,missing-method
setMethod("plot", c(x = "AoristicSum", y = "missing"), plot.AoristicSum)

## RateOfChange ----------------------------------------------------------------
#' @export
#' @method autoplot RateOfChange
autoplot.RateOfChange <- function(object, ..., level = 0.95, facet = TRUE) {
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
    aes_plot <- ggplot2::aes(x = .data$x, y = .data$y,
                             fill = .data$group, colour = .data$group)
  }
  if (anyNA(data$group)) {
    facet <- NULL
    aes_plot <- ggplot2::aes(x = .data$x, y = .data$y)
  }

  ggplot2::ggplot(data = data, mapping = aes_plot) +
    ggplot2::geom_hline(yintercept = 0, linetype = 3) +
    ggplot2::stat_summary(
      geom = "ribbon",
      fun.data = ggplot2::mean_cl_normal, # Needs Hmisc
      fun.args = list(conf.int = level),
      alpha = 0.5,
      colour = NA
    ) +
    ggplot2::stat_summary(geom = "line", fun = mean) +
    ggplot2::stat_summary(geom = "point", fun = mean) +
    ggplot2::scale_x_continuous(name = "Year CE") +
    ggplot2::scale_y_continuous(name = "Rate of Change") +
    facet
}

#' @export
#' @rdname plot_aoristic
#' @aliases autoplot,RateOfChange-method
setMethod("autoplot", "RateOfChange", autoplot.RateOfChange)

#' @export
#' @method plot RateOfChange
plot.RateOfChange <- function(x, level = 0.95, facet = TRUE, ...) {
  gg <- autoplot(object = x, level = level, facet = facet) +
    ggplot2::theme_bw()
  print(gg)
  invisible(x)
}

#' @export
#' @rdname plot_aoristic
#' @aliases plot,RateOfChange,missing-method
setMethod("plot", c(x = "RateOfChange", y = "missing"), plot.RateOfChange)

## Prepare data ----------------------------------------------------------------
## Build a long table for ggplot2
## * Must return a data.frame
## * Must preserve original ordering

prepare_aoristic <- function(object) {
  aoristic <- object
  blocks <- get_dates(object)
  groups <- object@groups

  grp <- unique(groups)
  data.frame(
    x = rowMeans(blocks),
    y = as.vector(aoristic),
    group = rep(grp, each = nrow(aoristic))
  )
}

prepare_roc <- function(object) {
  rates <- object
  breaks <- get_dates(object)

  m <- dim(rates)[[1]]
  p <- dim(rates)[[2]]
  q <- dim(rates)[[3]]
  n <- m * p

  grp <- dimnames(rates)[[3]]
  if (is.null(grp)) grp <- rep(NA_character_, q)

  data.frame(
    x = rep(rep(breaks, each = m), q),
    y = as.vector(rates),
    group = rep(grp, each = n)
  )
}
