# BOOTSTRAP
#' @include AllGenerics.R
NULL

# MeanDate =====================================================================
#' @export
#' @rdname bootstrap.MeanDate
#' @aliases bootstrap,MeanDate-method
setMethod(
  f = "bootstrap",
  signature = c(object = "MeanDate"),
  definition = function(object, n = 1000, f = NULL, level = 0.95,
                        interval = c("basic", "normal", "percentiles"),
                        seed = NULL, calendar = get_calendar()) {
    ## Validation
    interval <- match.arg(interval, several.ok = FALSE)

    ## Seed
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      stats::runif(1)
    if (is.null(seed)) {
      RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    } else {
      R.seed <- get(".Random.seed", envir = .GlobalEnv)
      set.seed(seed)
      RNGstate <- structure(seed, kind = as.list(RNGkind()))
      on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }

    m <- nrow(object)
    p <- ncol(object)
    seq_col <- seq_len(p)
    dates <- aion::as_year(object@dates, calendar = calendar)

    results <- vector(mode = "list", length = m)
    for (i in seq_len(m)) {
      hat <- .mcd(object[i, , 1, drop = TRUE], dates)
      spl <- t(arkhe::resample_multinomial(object[i, , 1, drop = TRUE], n = n))
      res <- apply(X = spl, MARGIN = 2, FUN = .mcd, dates = dates)
      if (is.function(f)) {
        results[[i]] <- f(res)
      } else {
        results[[i]] <- summary_bootstrap(res, hat, level = level, interval = interval)
      }
    }

    results <- do.call(rbind, results)
    results <- as.data.frame(results)
    rownames(results) <- rownames(object)
    attr(results, "seed") <- RNGstate
    results
  }
)

summary_bootstrap <- function(x, hat, level = 0.95, interval = "basic") {
  n <- length(x)
  boot_mean <- mean(x)
  boot_bias <- boot_mean - hat
  boot_error <- stats::sd(x)

  ci <- arkhe::confidence_bootstrap(x, level = level, t0 = hat, type = interval)
  results <- c(hat, boot_mean, boot_bias, boot_error, ci)
  names(results) <- c("original", "mean", "bias", "error", "lower", "upper")
  results
}

# EventDate ====================================================================
#' @export
#' @rdname bootstrap.EventDate
#' @aliases bootstrap,EventDate-method
setMethod(
  f = "bootstrap",
  signature = c(object = "EventDate"),
  definition = function(object, level = 0.95, probs = c(0.05, 0.95), n = 1000,
                        calendar = get_calendar(),
                        progress = getOption("kairos.progress"), ...) {
    ## Get data
    fit_model <- object@model
    fit_dim <- object@keep

    ## Partial bootstrap CA
    ## /!\ Be careful: EventDate inherits from CA
    ca_boot <- methods::callNextMethod(object, n = n)
    ca_rep_row <- dimensio::get_replications(ca_boot, margin = 1)

    ## Bootstrap assemblages
    event_rows <- apply(
      X = ca_rep_row,
      MARGIN = 1,
      FUN = function(x, axes, model, level, probs) {
        compute_date_boot(t(x), axes, model, level, probs)
      },
      axes = fit_dim,
      model = fit_model,
      level = level,
      probs = probs
    )

    results <- as.data.frame(t(event_rows))
    if (is.null(calendar)) return(results)

    results[] <- lapply(
      X = results,
      FUN = aion::as_year,
      calendar = calendar,
      decimal = TRUE
    )
    results
  }
)

#' Bootstrap Resampling of Assemblages
#'
#' Computes date event bootstraped statistics for each replicated sample.
#' @param x A [`numeric`] matrix of bootstrap replicates.
#' @param axes A [`numeric`] vector giving the subscripts of the CA components
#'  to keep.
#' @param model An object of class [`lm`][stats::lm()].
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param probs A [`numeric`] vector of probabilities with values in \eqn{[0,1]}
#'  (see [stats::quantile()]).
#' @return A [`numeric`] vector with the following elements:
#'  \describe{
#'   \item{`min`}{Minimum value.}
#'   \item{`mean`}{Mean value (event date).}
#'   \item{`max`}{Maximum value.}
#'   \item{`Q05`}{Sample quantile to 0.05 probability.}
#'   \item{`Q95`}{Sample quantile to 0.95 probability.}
#'  }
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
compute_date_boot <- function(x, axes, model, level, probs = c(0.05, 0.95)) {
  ## Remove missing values
  coords <- stats::na.omit(x[, axes])

  ## Gaussian multiple linear regression model
  event <- compute_event(model, coords, level)[, 1]
  Q <- stats::quantile(event, probs = probs, names = FALSE)

  distrib <- c(min(event), mean(event), max(event), Q)
  quant <- paste0("Q", round(probs * 100, 0))
  names(distrib) <- c("min", "mean", "max", quant)
  return(distrib)
}
