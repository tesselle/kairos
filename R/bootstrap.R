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
  definition = function(object, n = 1000, f = NULL, calendar = get_calendar()) {

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
        counts = w[i, , 1, drop = TRUE],
        dates = dates,
        f = f
      )
    }

    results <- do.call(rbind, results)
    rownames(results) <- rownames(w)
    as.data.frame(results)
  }
)

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
#'  (see [stats:quantile()]).
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
