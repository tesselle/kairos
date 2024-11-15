# PLOT DATES
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @method plot EventDate
plot.EventDate <- function(x, type = c("activity", "tempo"), event = FALSE,
                           calendar = getOption("kairos.calendar"),
                           select = 1, n = 500, eps = 1e-09,
                           col.accumulation = "black", col.event = "red",
                           flip = FALSE, ncol = NULL,
                           xlab = NULL, ylab = NULL,
                           main = NULL, sub = NULL,
                           ann = graphics::par("ann"), axes = TRUE,
                           frame.plot = axes, ...) {
  ## Validation
  type <- match.arg(type, several.ok = FALSE)

  ## Event date
  date_event <- density_event(x, n = n)
  date_range <- aion::time(date_event)
  cases <- colnames(date_event)

  ## Accumulation time
  date_acc <- density_accumulation(x, dates = date_range, type = type, n = n)

  ## Selection
  if (is.null(select)) index <- seq_along(cases)
  else if (is.character(select)) index <- which(cases %in% select)
  else index <- as.numeric(select)

  k <- length(index)
  if (k == 0) stop(tr_("Wrong selection."), call. = FALSE)

  if (type != "activity" || !event) {
    date_event <- array(data = NA, dim = list(n, k, 1))
  }

  date_event <- date_event[, index, , drop = FALSE]
  date_acc <- date_acc[, index, , drop = FALSE]

  ## Cleaning
  # date_acc[date_acc < eps] <- NA
  date_event[date_event < eps] <- NA
  na_event <- apply(date_event, 1, function(x) all(is.na(x)))
  na_acc <- apply(date_acc, 1, function(x) all(is.na(x)))
  date_drop <- na_event & na_acc

  ## Time series
  ts <- array(data = c(date_acc, date_event), dim = c(n, k, 2))
  dimnames(ts) <- list(NULL, cases[index], c("accumulation", "event"))
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

  col <- c(col.accumulation, grDevices::adjustcolor(col.event, alpha = 0.5))
  aion::plot(ts, panel = panel, calendar = calendar,
             flip = flip, ncol = ncol, xlab = xlab, ylab = ylab,
             main = main, sub = sub, ann = ann, axes = axes,
             frame.plot = frame.plot,
             col = col, lty = c(0, 0))

  invisible(x)
}

#' @export
#' @rdname plot_event
#' @aliases plot,EventDate,missing-method
setMethod("plot", c(x = "EventDate", y = "missing"), plot.EventDate)
