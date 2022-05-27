# DATE MODEL
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname event
#' @aliases event,data.frame,numeric-method
setMethod(
  f = "event",
  signature = signature(object = "data.frame", dates = "numeric"),
  definition = function(object, dates, cutoff = 90, level = 0.95, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, dates, cutoff = cutoff, level = level, ...)
  }
)
#' @export
#' @rdname event
#' @aliases event,matrix,numeric-method
setMethod(
  f = "event",
  signature = signature(object = "matrix", dates = "numeric"),
  definition = function(object, dates, cutoff = 90, level = 0.95, ...) {
    ## Validation
    cutoff <- as.integer(cutoff)
    if (cutoff < 50)
      stop("Cutoff value is below 50%, you can't be serious.", call. = FALSE)

    ## Correspondance analysis
    ## CA computation may rise error (if rows/columns filled only with zeros)
    results_CA <- dimensio::ca(object, ...)
    eig <- dimensio::get_eigenvalues(results_CA)
    keep_dim <- which(eig[, 3] <= cutoff)

    row_coord <- dimensio::get_coordinates(results_CA, margin = 1)
    row_coord <- row_coord[, keep_dim]

    ## Gaussian multiple linear regression model
    contexts <- bind_by_names(row_coord, dates)
    colnames(contexts)[1] <- "date"
    fit <- stats::lm(date ~ ., data = contexts, na.action = stats::na.omit)

    .EventDate(
      results_CA,
      dates = contexts$date,
      model = fit,
      cutoff = cutoff,
      keep = keep_dim
    )
  }
)

# Event ========================================================================
#' @export
#' @rdname event
#' @aliases predict_event,EventDate,missing-method
setMethod(
  f = "predict_event",
  signature = signature(object = "EventDate", data = "missing"),
  definition = function(object, margin = 1, level = 0.95) {
    data <- object@data
    methods::callGeneric(object, data = data, margin = margin, level = level)
  }
)

#' @export
#' @rdname event
#' @aliases predict_event,EventDate,matrix-method
setMethod(
  f = "predict_event",
  signature = signature(object = "EventDate", data = "matrix"),
  definition = function(object, data, margin = 1, level = 0.95) {
    ## Correspondance analysis
    ca_coord <- dimensio::predict(object, data, margin = margin)

    ## Predict event date
    fit_model <- get_model(object)
    ca_event <- compute_event(fit_model, ca_coord, level = level)

    # FIXME: error propagation
    # TODO: check predicted dates consistency

    as.data.frame(ca_event)
  }
)

# Accumulation =================================================================
#' @export
#' @rdname event
#' @aliases predict_accumulation,EventDate,missing-method
setMethod(
  f = "predict_accumulation",
  signature = signature(object = "EventDate", data = "missing"),
  definition = function(object) {
    data <- object@data
    methods::callGeneric(object, data = data)
  }
)

#' @export
#' @rdname event
#' @aliases predict_accumulation,EventDate,matrix-method
setMethod(
  f = "predict_accumulation",
  signature = signature(object = "EventDate", data = "matrix"),
  definition = function(object, data) {
    ## Predict event date
    col_event <- predict_event(object, data, margin = 2)

    # Accumulation time point estimate
    mcd(data, col_event$date)
  }
)

#' Predict event dates
#'
#' @param fit A \code{\link[stats:lm]{multiple linear model}}.
#' @param data A [`numeric`] matrix giving the coordinates in CA
#'  space.
#' @param level A length-one [`numeric`] vector giving the
#'  confidence level.
#' @return
#'  A four columns [`numeric`] matrix giving the predicted
#'  event dates, the corresponding confidence interval boundaries and the
#'  standard error of the predicted dates.
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
  round(results, digits = getOption("kairos.precision"))
}
