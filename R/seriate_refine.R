# REFINE MATRIX SERIATION
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriation
#' @aliases refine,BootstrapCA,function-method
setMethod(
  f = "refine",
  signature = signature(object = "AveragePermutationOrder", cutoff = "function"),
  definition = function(object, cutoff, n = 30, margin = 1, axes = c(1, 2)) {
    ## Partial bootstrap CA
    ca_boot <- dimensio::bootstrap(object, n = n)
    ca_rep <- dimensio::get_replications(ca_boot, margin = margin)

    ## Compute convex hull
    hull <- apply(
      X = ca_rep,
      MARGIN = 1,
      FUN = function(x, axes) compute_chull(t(x), axes),
      axes = axes
    )

    ## Get convex hull maximal dimension length for each sample
    hull_length <- vapply(
      X = hull,
      FUN = function(x) max(stats::dist(x, method = "euclidean")),
      FUN.VALUE = double(1)
    )

    ## Get cutoff values
    limit <- cutoff(hull_length)

    ## Samples to be kept
    keep <- which(hull_length < limit)

    ## Bind hull vertices in a data.frame
    ids <- rep(seq_along(hull), times = vapply(hull, nrow, numeric(1)))
    coords <- do.call(rbind, hull)
    coords <- cbind(id = ids, coords)

    .RefineCA(
      hull = coords,
      length = hull_length,
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
