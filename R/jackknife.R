# JACKKNIFE
#' @include AllGenerics.R
NULL

# MeanDate =====================================================================
#' @export
#' @rdname jackknife.MeanDate
#' @aliases jackknife,MeanDate-method
setMethod(
  f = "jackknife",
  signature = c(object = "MeanDate"),
  definition = function(object, f = NULL, calendar = get_calendar()) {

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
        counts = w[i, , 1, drop = TRUE],
        dates = dates,
        f = f
      )
    }
    results <- do.call(rbind, results)
    results <- as.data.frame(results)
    rownames(results) <- rownames(w)
    results
  }
)

# EventDate ====================================================================
#' @export
#' @rdname jackknife.EventDate
#' @aliases jackknife,EventDate-method
setMethod(
  f = "jackknife",
  signature = c(object = "EventDate"),
  definition = function(object, level = 0.95,
                        calendar = get_calendar(),
                        progress = getOption("kairos.progress"),
                        verbose = getOption("kairos.verbose"), ...) {
    ## Get data
    fit_model <- object@model
    fit_dates <- time(object)
    fit_data <- dimensio::get_data(object)
    fit_dim <- object@keep

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
