# DATE MODEL
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname event
#' @aliases event,data.frame,numeric-method
setMethod(
  f = "event",
  signature = c(object = "data.frame", dates = "numeric"),
  definition = function(object, dates, rank = NULL, sup_row = NULL,
                        calendar = CE(), ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, dates, rank = rank, sup_row = sup_row,
                         calendar = calendar, ...)
  }
)

#' @export
#' @rdname event
#' @aliases event,matrix,numeric-method
setMethod(
  f = "event",
  signature = c(object = "matrix", dates = "numeric"),
  definition = function(object, dates, rank = NULL, sup_row = NULL,
                        calendar = CE(), ...) {
    ## Sample
    n <- nrow(object)

    ## Check dates
    if (arkhe::has_names(dates)) {
      i <- match(names(dates), rownames(object))
      old_dates <- dates
      dates <- rep(NA_real_, n)
      dates[i] <- old_dates
    }
    arkhe::assert_length(dates, nrow(object))
    if (all(is.na(dates))) {
      stop("", call. = FALSE)
    }

    ## Supplementary rows
    sup <- logical(n)
    sup[sup_row] <- TRUE

    data_ref <- object[!sup, , drop = FALSE]
    data_sup <- object[sup, , drop = FALSE]
    dates_ref <- dates[!sup]
    dates_sup <- dates[sup]

    ## Validation
    clean <- TRUE
    while (clean) {
      rm_col <- colSums(data_ref) < 5
      if (any(rm_col)) {
        data_ref <- data_ref[, !rm_col, drop = FALSE]
        data_sup <- data_sup[, !rm_col, drop = FALSE]
      }

      rm_row_ref <- rowSums(data_ref) < 5
      if (any(rm_row_ref)) {
        data_ref <- data_ref[!rm_row_ref, , drop = FALSE]
        dates_ref <- dates_ref[!rm_row_ref]
      }

      rm_row_sup <- rowSums(data_sup) < 5
      if (any(data_sup)) {
        data_sup <- data_sup[!rm_row_sup, , drop = FALSE]
        dates_sup <- dates_sup[!rm_row_sup]
      }

      if (!any(rm_col) & !any(rm_row_ref) & !any(rm_row_sup)) clean <- FALSE
    }

    data <- rbind(data_ref, data_sup)
    dates <- c(dates_ref, dates_sup)
    sup <- seq_along(dates_sup) + length(dates_ref)

    ## Correspondance analysis
    if (is.null(rank)) {
      tmp <- dimensio::ca(data, rank = NULL, sup_row = sup, ...)
      eig <- dimensio::get_eigenvalues(tmp)
      rank <- which.max(eig$cumulative >= 60)
    }
    rank <- min(rank, dim(data_ref) - 1)
    results_CA <- dimensio::ca(data, rank = rank, sup_row = sup, ...)

    ## Get row coordinates
    row_coord <- dimensio::get_coordinates(results_CA, margin = 1)

    ## Rata die
    ok <- !row_coord$.sup & !is.na(dates)
    rd <- aion::fixed(dates[ok], calendar = calendar)

    ## Gaussian multiple linear regression model
    contexts <- data.frame(
      date = rd,
      row_coord[ok, -ncol(row_coord), drop = FALSE]
    )
    fit <- stats::lm(date ~ ., data = contexts)

    ## FIXME: temporary workaround until aion::fixed() can deal with NA
    dates_na <- is.na(dates)
    rt <- rep(NA_real_, length(dates))
    rt[!dates_na] <- aion::fixed(dates[!dates_na], calendar = calendar)
    rt <- aion::as_fixed(rt)

    .EventDate(
      results_CA,
      dates = rt,
      model = fit,
      keep = seq_len(rank)
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
    if (margin == 1) data <- data[, !object@columns@supplement, drop = FALSE]
    if (margin == 2) data <- data[!object@rows@supplement, , drop = FALSE]
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
  definition = function(object, calendar = NULL) {
    data <- object@data
    data <- data[!object@rows@supplement, , drop = FALSE]
    methods::callGeneric(object, data = data, calendar = calendar)
  }
)

#' @export
#' @rdname predict_event
#' @aliases predict_accumulation,EventDate,matrix-method
setMethod(
  f = "predict_accumulation",
  signature = c(object = "EventDate", data = "matrix"),
  definition = function(object, data, level = 0.95, calendar = NULL) {
    ## Predict event date
    col_event <- predict_event(object, data, margin = 2, calendar = NULL)

    ## Accumulation time point estimate
    date_range <- seq(
      from = min(col_event$lower, na.rm = TRUE),
      to = max(col_event$upper, na.rm = TRUE),
      length.out = 1000
    )
    dens <- mapply(
      FUN = stats::dnorm,
      mean = col_event$date,
      sd = col_event$error,
      MoreArgs = list(date_range),
      SIMPLIFY = TRUE
    )
    acc <- apply(
      X = data,
      MARGIN = 1,
      FUN = function(x, density) {
        colSums(t(density) * as.numeric(x))
      },
      density = dens
    )

    alpha <- (1 - level) / 2
    probs <- c(0.5, alpha, 1 - alpha)
    quant <- quantile_density(x = date_range, y = acc, probs = probs)

    ## Convert
    quant <- as.data.frame(quant)
    colnames(quant) <- c("date", "lower", "upper")
    if (is.null(calendar)) return(quant)
    quant[] <- lapply(X = quant, FUN = aion::as_year, calendar = calendar)
    quant
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
