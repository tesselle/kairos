# PLOT DATES
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @method plot EventDate
plot.EventDate <- function(x, type = c("activity", "tempo"), event = FALSE,
                           calendar = getOption("kairos.calendar"),
                           select = 1, n = 500, eps = 1e-09,
                           flip = FALSE, ncol = NULL,
                           xlab = NULL, ylab = NULL,
                           main = NULL, sub = NULL,
                           ann = graphics::par("ann"), axes = TRUE,
                           frame.plot = axes, ...) {
  ## Validation
  type <- match.arg(type, several.ok = FALSE)
  n <- as.integer(n)

  ## Get data
  rows <- predict_event(x, margin = 1, calendar = NULL)
  row_dates <- rows$date
  row_lower <- rows$lower
  row_upper <- rows$upper
  row_errors <- rows$error

  columns <- predict_event(x, margin = 2, calendar = NULL)
  col_dates <- columns$date
  col_errors <- columns$error
  date_range <- seq(
    from = min(row_lower, na.rm = TRUE),
    to = max(row_upper, na.rm = TRUE),
    length.out = n
  )

  ## Selection
  cases <- rownames(rows)
  if (is.null(select)) index <- seq_along(cases)
  else if (is.character(select)) index <- which(cases %in% select)
  else index <- as.numeric(select)

  k <- length(index)
  if (k == 0) stop("Wrong selection.", call. = FALSE)

  ## Event date
  if (type == "activity" && event) {
    date_event <- mapply(
      FUN = stats::dnorm,
      mean = row_dates[index],
      sd = row_errors[index],
      MoreArgs = list(x = date_range),
      SIMPLIFY = TRUE
    )
    colnames(date_event) <- cases[index]
  } else {
    date_event <- matrix(data = NA, nrow = n, ncol = k)
  }

  ## Accumulation time
  ## Weighted sum of the fabric dates
  counts <- dimensio::get_data(x)[index, , drop = FALSE]
  freq <- counts / rowSums(counts)
  ## Tempo vs activity plot
  fun <- switch(
    type,
    activity = stats::dnorm,
    tempo = stats::pnorm
  )
  col_density <- mapply(
    FUN = fun,
    mean = col_dates,
    sd = col_errors,
    MoreArgs = list(date_range),
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
  # date_acc <- date_acc / colSums(date_acc)

  ## Time series
  date_event[date_event < eps] <- NA
  date_acc[date_acc < eps] <- NA
  date_drop <- apply(date_event, 1, function(x) all(is.na(x))) &
    apply(date_acc, 1, function(x) all(is.na(x)))
  ts <- array(data = c(date_acc, date_event), dim = c(n, k, 2),
              dimnames = list(NULL, cases[index], c("accumulation", "event")))
  ts <- aion::series(object = ts[!date_drop, , , drop = FALSE],
                     time = aion::as_fixed(date_range[!date_drop]))

  panel <- switch(
    type,
    activity = function(x, y, ...) {
      graphics::polygon(x = c(x, rev(x)), y = c(y, rep(0, length(y))),
                        border = NA, ...)
      graphics::lines(x, y, col = "black", lty = list(...)$lty)
    },
    tempo = function(x, y, ...) graphics::lines(x, y, col = "black", lty = 1)
  )

  aion::plot(ts, panel = panel, calendar = calendar,
             flip = flip, ncol = ncol, xlab = xlab, ylab = ylab,
             main = main, sub = sub, ann = ann, axes = axes,
             frame.plot = frame.plot,
             col = c("lightgrey", NA), lty = c(3, 1))
}

#' @export
#' @rdname plot_event
#' @aliases plot,EventDate,missing-method
setMethod("plot", c(x = "EventDate", y = "missing"), plot.EventDate)
