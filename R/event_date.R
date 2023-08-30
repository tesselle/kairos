# DATE MODEL
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname event
#' @aliases event,data.frame,numeric-method
setMethod(
  f = "event",
  signature = c(object = "data.frame", dates = "numeric"),
  definition = function(object, dates, rank = 10, calendar = CE(), ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, dates, rank = rank, calendar = calendar, ...)
  }
)

#' @export
#' @rdname event
#' @aliases event,matrix,numeric-method
setMethod(
  f = "event",
  signature = c(object = "matrix", dates = "numeric"),
  definition = function(object, dates, rank = 10, calendar = CE(), ...) {
    ## Correspondance analysis
    ## CA computation may rise error (if rows/columns filled only with zeros)
    results_CA <- dimensio::ca(object, rank = rank, ...)
    eig <- dimensio::get_eigenvalues(results_CA)

    keep_dim <- seq_len(nrow(eig))
    row_coord <- dimensio::get_coordinates(results_CA, margin = 1)
    row_coord <- row_coord[, keep_dim]

    ## Rata die
    i <- match(names(dates), rownames(row_coord))
    dates <- aion::fixed(dates, calendar = calendar)

    ## Gaussian multiple linear regression model
    contexts <- row_coord[i, , drop = FALSE]
    data <- data.frame(date = dates, contexts)
    fit <- stats::lm(date ~ ., data = data)

    .EventDate(
      results_CA,
      contexts = object[i, , drop = FALSE],
      dates = dates,
      model = fit,
      keep = keep_dim
    )
  }
)

# Event ========================================================================
#' @export
#' @rdname predict_event
#' @aliases predict_event,EventDate,missing-method
setMethod(
  f = "predict_event",
  signature = c(object = "EventDate", data = "missing"),
  definition = function(object, margin = 1, level = 0.95, calendar = NULL) {
    data <- object@data
    methods::callGeneric(object, data = data, margin = margin, level = level,
                         calendar = calendar)
  }
)

#' @export
#' @rdname predict_event
#' @aliases predict_event,EventDate,matrix-method
setMethod(
  f = "predict_event",
  signature = c(object = "EventDate", data = "matrix"),
  definition = function(object, data, margin = 1, level = 0.95,
                        calendar = NULL) {
    ## Correspondence analysis
    ca_coord <- dimensio::predict(object, data, margin = margin)

    ## Predict event date
    fit_model <- object@model
    ca_event <- compute_event(fit_model, ca_coord, level = level)

    # FIXME: error propagation
    # TODO: check predicted dates consistency

    ## Convert
    ca_event <- as.data.frame(ca_event)
    if (is.null(calendar)) return(ca_event)
    ca_event[] <- lapply(X = ca_event, FUN = aion::as_year, calendar = calendar)
    ca_event
  }
)

# Accumulation =================================================================
#' @export
#' @rdname predict_event
#' @aliases predict_accumulation,EventDate,missing-method
setMethod(
  f = "predict_accumulation",
  signature = c(object = "EventDate", data = "missing"),
  definition = function(object) {
    data <- object@data
    methods::callGeneric(object, data = data)
  }
)

#' @export
#' @rdname predict_event
#' @aliases predict_accumulation,EventDate,matrix-method
setMethod(
  f = "predict_accumulation",
  signature = c(object = "EventDate", data = "matrix"),
  definition = function(object, data) {
    ## Predict event date
    col_event <- predict_event(object, data, margin = 2, calendar = NULL)

    # Accumulation time point estimate
    acc <- mcd(data, col_event$date, calendar = NULL)

    as.data.frame(acc)
  }
)

#' Predict event dates
#'
#' @param fit A \code{\link[stats:lm]{multiple linear model}}.
#' @param data A [`numeric`] matrix giving the coordinates in CA space.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @return
#'  A four columns [`numeric`] matrix giving the predicted event dates,
#'  the corresponding confidence interval boundaries and the standard error of
#'  the predicted dates.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
compute_event <- function(fit, data, level) {
  data <- as.data.frame(data)

  date_predict <- stats::predict.lm(fit, data, se.fit = TRUE,
                                    interval = "confidence", level = level)
  results <- cbind(
    date_predict$fit, # Three columns matrix: predicted value + CI boundaries
    date_predict$se.fit
  )
  rownames(results) <- rownames(data)
  colnames(results) <- c("date", "lower", "upper", "error")
  results
}
