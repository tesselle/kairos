# SHOW METHODS
#' @include AllClasses.R
NULL

# PermutationOrder =============================================================
setMethod(
  f = "show",
  signature = "PermutationOrder",
  definition = function(object) {
    k <- 50
    rows <- strtrim(paste0(object[["rows"]], collapse = " "), k)
    columns <- strtrim(paste0(object[["columns"]], collapse = " "), k)
    cat(
      sprintf("<%s: %s>", class(object), object[["method"]]),
      "Permutation order for matrix seriation:",
      sprintf("- Row order: %s", paste0(rows, "...")),
      sprintf("- Column order: %s", paste0(columns, "...")),
      sep = "\n"
    )
  }
)
