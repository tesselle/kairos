# AORISTIC ANALYSIS
#' @include AllClasses.R AllGenerics.R
NULL

# Aoristic sum =================================================================
#' @export
#' @rdname aoristic
#' @aliases sum_aoristic,numeric,numeric-method
setMethod(
  f = "sum_aoristic",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, step = 1, start = min(x, na.rm = TRUE),
                        stop = max(y, na.rm = TRUE), weight = TRUE,
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

    ## Grouping
    lvl <- unique(groups)
    grp <- factor(groups, levels = lvl, exclude = NULL)
    spl <- split(seq_along(x), f = grp)
    m <- length(spl)

    ## Weighting
    if (weight) {
      delta <- abs(y - x)
      # delta[delta == 0] <- 1 # FIXME: should investigate this
      z <- step / delta
    } else {
      z <- rep(1, n)
    }

    dates <- seq(from = start, to = stop, by = step)
    ao_probs <- array(data = 0, dim = c(n, length(dates), m))

    ## Aoristic probability
    for (k in seq_len(m)) {
      s <- spl[[k]]
      ao_probs[, , k] <- vapply(
        X = dates,
        FUN = function(x, from, to, weights) {
          w <- numeric(length(weights))
          i <- which(from <= x & to >= x)
          w[i] <- weights[i]
          w
        },
        FUN.VALUE = numeric(length(s)),
        from = x[s],
        to = y[s],
        weights = z[s]
      )
    }

    ## Aoristic sum
    ao_sum <- apply(X = ao_probs, MARGIN = 2, FUN = sum)
    ao_sum <- as.matrix(ao_sum)

    if (!anyNA(groups)) {
      dimnames(ao_probs)[3] <- rownames(ao_sum) <- lvl
    }

    .AoristicSum(
      from = x,
      to = y,
      weights = z,
      groups = groups,
      dates = dates,
      p = ao_probs,
      sum = ao_sum
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

# Rate of change ===============================================================
#' @export
#' @rdname roc
#' @aliases rate_roc,AoristicSum-method
setMethod(
  f = "rate_roc",
  signature = signature(object = "AoristicSum"),
  definition = function(object, n = 100) {
    ## Get time span of the blocks
    dates <- object[["dates"]]
    step <- unique(diff(dates))

    ## Get aoristic weights
    w <- object[["p"]]

    m <- dim(w)[1]
    p <- dim(w)[2]
    q <- dim(w)[3]

    ## Monte Carlo simulation
    roc <- array(data = 0, dim = c(n, p - 1, q))
    for (j in seq_len(q)) {
      sim <- array(data = 0, dim = c(m, p, n))
      for (i in seq_len(m)) {
        probs <- w[i, , j]
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

    blocks <- paste(utils::head(dates, -1), utils::tail(dates, -1), sep = "_")

    .RateOfChange(
      blocks = blocks,
      rates = roc / step
    )
  }
)
