# AORISTIC ANALYSIS
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname aoristic
#' @aliases sum_aoristic,numeric,numeric-method
setMethod(
  f = "sum_aoristic",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, step = 1, start = min(x, na.rm = TRUE),
                        stop = max(y, na.rm = TRUE), weight = FALSE,
                        groups = NULL) {
    ## Validation
    n <- length(x)
    if (n != length(y)) {
      msg <- sprintf("%s must be of length %d; not %d.",
                     sQuote("y"), n, length(y))
      stop(msg, call. = FALSE)
    }
    if (!is.null(groups)) {
      if (n != length(groups)) {
        msg <- sprintf("%s must be of length %d; not %d.",
                       sQuote("groups"), n, length(groups))
        stop(msg, call. = FALSE)
      }
    } else {
      groups <- rep(NA_character_, n)
    }
    grp <- factor(groups, levels = unique(groups), exclude = NULL)
    spl <- split(seq_along(x), f = grp)
    m <- length(spl)

    ## Weighting
    if (weight) {
      delta <- abs(y - x)
      delta[delta == 0] <- 1
      z <- 1 / delta
    } else {
      z <- rep(1, n)
    }

    dates <- seq(from = start, to = stop, by = step)
    ao_sum <- vector(mode = "list", length = m)

    for (k in seq_len(m)) {
      s <- spl[[k]]
      ao_sum[[k]] <- vapply(
        X = dates,
        FUN = function(x, from, to, weights) {
          i <- which(from <= x & to >= x)
          sum(weights[i])
        },
        FUN.VALUE = numeric(1),
        from = x[s],
        to = y[s],
        weights = z[s]
      )
    }

    .AoristicSum(
      from = x,
      to = y,
      weights = z,
      groups = groups,
      dates = dates,
      sum = unlist(ao_sum)
    )
  }
)

#' @export
#' @rdname aoristic
#' @aliases sum_aoristic,list,missing-method
setMethod(
  f = "sum_aoristic",
  signature = signature(x = "list", y = "missing"),
  definition = function(x, step = 1, start = min(x$from, na.rm = TRUE),
                        stop = max(x$to, na.rm = TRUE), weight = FALSE,
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

    methods::callGeneric(x = x$from, y = x$to, step = step, start = start,
                         stop = stop, weight = weight, groups = g)
  }
)
