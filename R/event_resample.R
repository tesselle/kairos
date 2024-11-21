# REFINE DATE MODEL
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname resample_event
#' @aliases jackknife,EventDate-method
setMethod(
  f = "jackknife",
  signature = c(object = "EventDate"),
  definition = function(object, level = 0.95,
                        calendar = getOption("kairos.calendar"),
                        progress = getOption("kairos.progress"),
                        verbose = getOption("kairos.verbose"), ...) {
    ## Get data
    fit_model <- object@model
    fit_dates <- time(object)
    fit_data <- dimensio::get_data(object)
    fit_dim <- object@keep

    ## Compute event dates
    event <- predict_event(object, level = level, calendar = calendar)

    ## Jackknife model coefficients
    jack_values <- compute_date_jack(
      x = fit_data,
      dates = fit_dates,
      rank = length(fit_dim),
      progress = progress,
      verbose = verbose
    )
    jack_coef <- colMeans(jack_values)

    ## Change lm coefficients (UGLY)
    fit_model$coefficients <- jack_coef[c(1, fit_dim + 1)]

    ## Predict event date for each context
    row_coord <- dimensio::get_coordinates(object, margin = 1)
    jack_event <- compute_event(fit_model, row_coord, level = level, error = FALSE)

    results <- as.data.frame(jack_event)
    if (!is.null(calendar)) {
      results[] <- lapply(
        X = results,
        FUN = aion::as_year,
        calendar = calendar,
        decimal = TRUE
      )
    }
    results
  }
)

#' Jackknife Fabrics
#'
#' Compute date event jackknifed statistics for each replicated sample.
#' @param x A [`numeric`] matrix of count data.
#' @param dates A [`numeric`] vector of known dates.
#' @param rank A [`numeric`] value.
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @param ... Currently not used].
#' @return A `numeric` [`matrix`] of linear model coefficients.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
compute_date_jack <- function(x, dates, rank = 10,
                              progress = FALSE, verbose = FALSE, ...) {
  m <- ncol(x)
  k <- seq_len(m)
  jack <- vector(mode = "list", length = m)

  progress_bar <- interactive() && isTRUE(progress)
  if (progress_bar) pbar <- utils::txtProgressBar(max = m, style = 3)

  for (j in k) {
    counts <- x[, -j, drop = FALSE]
    model <- event(counts, dates = dates, calendar = NULL, rank = rank, verbose = verbose)
    jack[[j]] <- coef(model, calendar = NULL) # Get model coefficients
    if (progress_bar) utils::setTxtProgressBar(pbar, j)
  }

  if (progress_bar) close(pbar)
  do.call(rbind, jack)
}

#' @export
#' @rdname resample_event
#' @aliases bootstrap,EventDate-method
setMethod(
  f = "bootstrap",
  signature = c(object = "EventDate"),
  definition = function(object, level = 0.95, probs = c(0.05, 0.95), n = 1000,
                        calendar = getOption("kairos.calendar"),
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
