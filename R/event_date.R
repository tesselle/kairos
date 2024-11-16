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
  definition = function(object, dates, calendar = CE(),
                        rank = NULL, sup_row = NULL, total = 5,
                        verbose = getOption("kairos.verbose"), ...) {
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

    ## Supplementary rows
    sup <- logical(n)
    sup[sup_row] <- TRUE

    data_ref <- object[!sup, , drop = FALSE]
    data_sup <- object[sup, , drop = FALSE]
    dates_ref <- dates[!sup]
    dates_sup <- dates[sup]

    ## Cleansing
    data_clean <- clean_total(data_ref, data_sup, dates_ref, dates_sup,
                              total = total, verbose = verbose)

    data <- rbind(data_clean$data_ref, data_clean$data_sup)
    dates <- c(data_clean$dates_ref, data_clean$dates_sup)
    sup <- seq_along(data_clean$dates_sup) + length(data_clean$dates_ref)

    ## Correspondance analysis
    if (is.null(rank)) {
      tmp <- dimensio::ca(data, rank = NULL, sup_row = sup, ...)
      eig <- dimensio::get_eigenvalues(tmp)
      rank <- which.max(eig$cumulative >= 60)
    }
    rank <- min(rank, dim(data) - 1)
    results_CA <- dimensio::ca(data, rank = rank, sup_row = sup, ...)

    ## Get row coordinates
    row_coord <- dimensio::get_coordinates(results_CA, margin = 1)

    ## Rata die
    if (is.null(calendar)) {
      rd <- aion::as_fixed(data_clean$dates_ref)
    } else {
      rd <- aion::fixed(data_clean$dates_ref, calendar = calendar)
    }

    ## Gaussian multiple linear regression model
    contexts <- data.frame(
      date = rd,
      row_coord[!row_coord$.sup, -ncol(row_coord), drop = FALSE]
    )
    fit <- stats::lm(date ~ ., data = contexts)

    ## FIXME: temporary workaround until aion::fixed() can deal with NA
    dates_not_na <- which(!is.na(dates))
    rt <- rep(NA_real_, length(dates))
    if (is.null(calendar)) {
      rt[dates_not_na] <- aion::as_fixed(dates[dates_not_na])
    } else {
      rt[dates_not_na] <- aion::fixed(dates[dates_not_na], calendar = calendar)
    }
    rt <- aion::as_fixed(rt)

    .EventDate(
      results_CA,
      dates = rt,
      model = fit,
      keep = seq_len(rank)
    )
  }
)

clean_total <- function(data_ref, data_sup, dates_ref, dates_sup,
                        total = 5, verbose = TRUE) {

  clean <- TRUE
  n_rm_col <- n_rm_ref <- n_rm_sup <- 0

  while (clean) {
    rm_col <- colSums(data_ref) < total
    n_rm_col <- n_rm_col + sum(rm_col)
    if (any(rm_col)) {
      data_ref <- data_ref[, !rm_col, drop = FALSE]
      data_sup <- data_sup[, !rm_col, drop = FALSE]
    }

    rm_row_ref <- rowSums(data_ref) < total
    n_rm_ref <- n_rm_ref + sum(rm_row_ref)
    if (any(rm_row_ref)) {
      data_ref <- data_ref[!rm_row_ref, , drop = FALSE]
      dates_ref <- dates_ref[!rm_row_ref]
    }

    rm_row_sup <- rowSums(data_sup) < total
    n_rm_sup <- n_rm_sup + sum(rm_row_sup)
    if (any(data_sup)) {
      data_sup <- data_sup[!rm_row_sup, , drop = FALSE]
      dates_sup <- dates_sup[!rm_row_sup]
    }

    if (!any(rm_col) & !any(rm_row_ref) & !any(rm_row_sup)) clean <- FALSE
  }

  if (isTRUE(verbose)) {
    if (n_rm_col > 0) {
      msg <- ngettext(n_rm_col,
                      "%d column has a grand total of less than %d.",
                      "%d columns have a grand total of less than %d.")
      message(sprintf(msg, n_rm_col, total))
    }
    if (n_rm_ref > 0) {
      msg <- ngettext(n_rm_ref,
                      "%d row has a grand total of less than %d.",
                      "%d rows have a grand total of less than %d.")
      message(sprintf(msg, n_rm_ref, total))
    }
    if (n_rm_sup > 0) {
      msg <- ngettext(n_rm_sup,
                      "%d supplementary row has a grand total of less than %d.",
                      "%d supplementary rows have a grand total of less than %d.")
      message(sprintf(msg, n_rm_sup, total))
    }
    n_rm <- n_rm_col + n_rm_ref + n_rm_sup
    if (n_rm > 0) {
      msg <- ngettext(n_rm, "It was omitted from the analysis.",
                      "They were omitted from the analysis.")
      message(msg)
    }
  }

  if (all(is.na(dates_ref))) {
    stop(tr_("All dates are missing!"), call. = FALSE)
  }

  list(
    data_ref = data_ref,
    data_sup = data_sup,
    dates_ref = dates_ref,
    dates_sup = dates_sup
  )
}

# Event ========================================================================
#' @export
#' @rdname predict_event
#' @aliases predict_event,EventDate,missing-method
setMethod(
  f = "predict_event",
  signature = c(object = "EventDate", data = "missing"),
  definition = function(object, margin = 1, level = 0.95,
                        calendar = getOption("kairos.calendar")) {
    data <- dimensio::get_coordinates(object, margin = margin)
    data <- as.matrix(data[, -ncol(data), drop = FALSE])

    .predict_event(object, data, level = level, calendar = calendar)
  }
)

#' @export
#' @rdname predict_event
#' @aliases predict_event,EventDate,matrix-method
setMethod(
  f = "predict_event",
  signature = c(object = "EventDate", data = "matrix"),
  definition = function(object, data, margin = 1, level = 0.95,
                        calendar = getOption("kairos.calendar")) {
    ## Correspondence analysis
    data <- dimensio::predict(object, data, margin = margin)

    .predict_event(object, data, level = level, calendar = calendar)
  }
)

.predict_event <- function(object, data, level = 0.95,
                           calendar = getOption("kairos.calendar")) {
  ## Predict event date
  fit_model <- object@model
  ca_event <- compute_event(fit_model, data, level = level)

  # TODO: check predicted dates consistency

  ## Convert
  ca_event <- as.data.frame(ca_event)
  if (is.null(calendar)) return(ca_event)

  ca_event[] <- lapply(
    X = ca_event,
    FUN = aion::as_year,
    calendar = calendar,
    decimal = TRUE
  )
  ca_event
}

#' @export
#' @rdname density_event
#' @aliases density_event,EventDate-method
setMethod(
  f = "density_event",
  signature = c(object = "EventDate"),
  definition = function(object, dates = NULL, calendar = NULL, n = 500, ...) {
    ## Get data
    rows <- predict_event(object, margin = 1, calendar = NULL)
    row_dates <- rows$date
    row_errors <- rows$error

    if (is.null(dates)) {
      dates <- seq(
        from = min(rows$lower, na.rm = TRUE) * 0.96,
        to = max(rows$upper, na.rm = TRUE) * 1.04,
        length.out = n
      )
    }
    if (is.null(calendar)) {
      dates <- aion::as_fixed(dates)
    } else {
      dates <- aion::fixed(dates, calendar = calendar)
    }

    ## Density estimation
    date_event <- mapply(
      FUN = stats::dnorm,
      mean = row_dates,
      sd = row_errors,
      MoreArgs = list(x = dates),
      SIMPLIFY = TRUE
    )

    colnames(date_event) <- rownames(rows)
    aion::series(object = date_event, time = dates)
  }
)

# Accumulation =================================================================
#' @export
#' @rdname predict_event
#' @aliases predict_accumulation,EventDate,missing-method
setMethod(
  f = "predict_accumulation",
  signature = c(object = "EventDate", data = "missing"),
  definition = function(object, level = 0.95,
                        calendar = getOption("kairos.calendar")) {
    data <- object@data
    methods::callGeneric(object, data = data, level = level, calendar = calendar)
  }
)

#' @export
#' @rdname predict_event
#' @aliases predict_accumulation,EventDate,matrix-method
setMethod(
  f = "predict_accumulation",
  signature = c(object = "EventDate", data = "matrix"),
  definition = function(object, data, level = 0.95,
                        calendar = getOption("kairos.calendar")) {
    ## Predict event date
    col_event <- predict_event(object, margin = 2, calendar = NULL)

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

    quant[] <- lapply(
      X = quant,
      FUN = aion::as_year,
      calendar = calendar,
      decimal = TRUE
    )
    quant
  }
)

#' @export
#' @rdname density_event
#' @aliases density_accumulation,EventDate-method
setMethod(
  f = "density_accumulation",
  signature = c(object = "EventDate"),
  definition = function(object, dates = NULL, calendar = NULL,
                        type = c("activity", "tempo"), n = 500, ...) {
    ## Validation
    type <- match.arg(type, several.ok = FALSE)

    ## Get data
    columns <- predict_event(object, margin = 2, calendar = NULL)
    col_dates <- columns$date
    col_errors <- columns$error

    if (is.null(dates)) {
      dates <- seq(
        from = min(columns$lower, na.rm = TRUE) * 0.96,
        to = max(columns$upper, na.rm = TRUE) * 1.04,
        length.out = n
      )
    }
    if (is.null(calendar)) {
      dates <- aion::as_fixed(dates)
    } else {
      dates <- aion::fixed(dates, calendar = calendar)
    }

    ## Weighted sum of the fabric dates
    counts <- dimensio::get_data(object)
    freq <- counts / rowSums(counts)

    ## Tempo vs activity plot
    fun <- switch(
      type,
      activity = stats::dnorm,
      tempo = stats::pnorm
    )

    ## Density estimation
    col_density <- mapply(
      FUN = fun,
      mean = col_dates,
      sd = col_errors,
      MoreArgs = list(dates),
      SIMPLIFY = TRUE
    )

    date_acc <- apply(
      X = freq,
      MARGIN = 1,
      FUN = function(x, density) {
        colSums(t(density) * as.numeric(x))
      },
      density = col_density
    )

    aion::series(object = date_acc, time = dates)
  }
)

# Helpers ======================================================================
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
compute_event <- function(fit, data, level, error = TRUE) {
  data <- as.data.frame(data)

  date_predict <- stats::predict.lm(
    object = fit,
    newdata = data,
    se.fit = TRUE,
    interval = "confidence",
    level = level
  )

  # Three columns matrix: predicted value + CI boundaries
  results <- as.data.frame(date_predict$fit)
  rownames(results) <- rownames(data)
  colnames(results) <- c("date", "lower", "upper")
  if (error) results$error <- date_predict$se.fit

  results
}
