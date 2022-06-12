# REFINE MATRIX SERIATION
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriation
#' @aliases refine,CA-method
setMethod(
  f = "refine",
  signature = signature(object = "AveragePermutationOrder"),
  definition = function(object, cutoff, margin = c(1, 2), axes = c(1, 2), n = 30) {
    ## Partial bootstrap CA
    ## /!\ Be careful: AveragePermutationOrder inherits from CA
    object <- dimensio::bootstrap(object, n = n)
    methods::callGeneric(object, cutoff = cutoff, margin = margin, axes = axes)
  }
)

#' @export
#' @rdname seriation
#' @aliases refine,BootstrapCA-method
setMethod(
  f = "refine",
  signature = signature(object = "BootstrapCA"),
  definition = function(object, cutoff, margin = 1, axes = c(1, 2)) {
    ## Validation
    if (!is.function(cutoff)) {
      stop(sQuote("cutoff"), " must be a function.", call. = FALSE)
    }

    ca_rep <- dimensio::get_replications(object, margin = margin)

    ## Compute convex hull
    hull <- apply(
      X = ca_rep,
      MARGIN = 1,
      FUN = function(x, axes) compute_chull(t(x), axes),
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

    ## Samples to be kept
    keep <- which(len < limit)

    ## Bind hull vertices in a data.frame
    ids <- rep(seq_along(hull), times = vapply(hull, nrow, numeric(1)))
    coords <- do.call(rbind, hull)
    coords <- cbind(id = ids, coords)

    .RefineCA(
      hull = coords,
      length = len,
      keep = keep,
      cutoff = limit,
      margin = as.integer(margin)
    )
  }
)

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
