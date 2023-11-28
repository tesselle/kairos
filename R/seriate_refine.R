# REFINE MATRIX SERIATION
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriate_refine
#' @aliases seriate_refine,AveragePermutationOrder-method
setMethod(
  f = "seriate_refine",
  signature = c(object = "AveragePermutationOrder"),
  definition = function(object, cutoff, margin = 1, axes = 1, n = 30, ...) {
    ## Partial bootstrap CA
    ## /!\ Be careful: AveragePermutationOrder inherits from CA
    object <- dimensio::bootstrap(object, n = n)
    methods::callGeneric(object, cutoff = cutoff, margin = margin, axes = axes, ...)
  }
)

#' @export
#' @rdname seriate_refine
#' @aliases seriate_refine,BootstrapCA-method
setMethod(
  f = "seriate_refine",
  signature = c(object = "BootstrapCA"),
  definition = function(object, cutoff, margin = 1, axes = 1, ...) {
    ## Validation
    if (!is.function(cutoff)) {
      stop(sQuote("cutoff"), " must be a function.", call. = FALSE)
    }

    ca_rep <- dimensio::get_replications(object, margin = margin)

    ## Compute convex hull
    hull <- apply(
      X = ca_rep,
      MARGIN = 1,
      FUN = function(x, axes) compute_chull(t(x), c(1, 2)),
      axes = axes
    )

    ## Get convex hull maximal dimension length for each sample
    len <- vapply(
      X = hull,
      FUN = function(x) max(stats::dist(x, method = "euclidean")),
      FUN.VALUE = double(1)
    )

    ## Get cutoff values
    limit <- cutoff(len)

    ## Samples to be excluded
    sup <- which(len >= limit)

    ## Seriation
    ser <- seriate_average(dimensio::get_data(object), margin = c(1, 2),
                           axes = axes, sup_row = sup, ...)

    .RefinePermutationOrder(
      ser,
      length = len,
      cutoff = limit,
      keep = which(len < limit),
      margin = as.integer(margin)
    )
  }
)

#' @export
#' @method hist RefinePermutationOrder
hist.RefinePermutationOrder <- function(x, ...) {
  ## Get data
  cutoff <- x@cutoff

  graphics::hist(
    x = x@length,
    xlab = "Maximum length",
    main = NULL,
    las = 1,
    ...
  )
  graphics::abline(v = cutoff, col = "red")
  graphics::axis(side = 3, at = cutoff, labels = round(cutoff, 2), tick = FALSE,
                 col = "red", col.ticks = "red", col.axis = "red")

  invisible(x)
}

#' @export
#' @describeIn seriate_refine Compute and plot a histogram of convex hull
#'  maximum dimension length.
#' @aliases hist,RefinePermutationOrder-method
setMethod("hist", "RefinePermutationOrder", hist.RefinePermutationOrder)

#' Convex Hull of CA Coordinates
#'
#' Compute convex hull area for each replicated sample
#' @param x A [`numeric`] matrix of bootstrap replicates.
#' @param axes A length-two [`numeric`] vector giving the subscripts
#'  of the CA components to use.
#' @return A [`matrix`].
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
compute_chull <- function(x, axes) {
  # Remove missing values
  clean <- stats::na.omit(x[, axes])
  # Get convex hull coordinates
  points <- grDevices::chull(clean)
  # Repeat first point
  hull <- clean[c(points, points[1]), axes]
  colnames(hull) <- c("x", "y")

  return(hull)
}
