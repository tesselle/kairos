# SHOW METHODS
#' @include AllClasses.R
NULL

# PermutationOrder =============================================================
setMethod(
  f = "show",
  signature = "PermutationOrder",
  definition = function(object) {
    k <- 50
    rows <- strtrim(paste0(object@rows_order, collapse = " "), k)
    columns <- strtrim(paste0(object@columns_order, collapse = " "), k)
    cat(
      sprintf("<%s>", class(object)),
      "Permutation order for matrix seriation:",
      sprintf("- Row order: %s", paste0(rows, "...")),
      sprintf("- Column order: %s", paste0(columns, "...")),
      sep = "\n"
    )
  }
)

# RefineCA =====================================================================
setMethod(
  f = "show",
  signature = "RefinePermutationOrder",
  definition = function(object) {
    value <- switch (object@margin, `1` = "Rows", `2` = "Columns")
    keep <- length(object@keep)
    total <- length(object@length)
    pc <- round(keep * 100 / total)
    methods::callNextMethod(object)
    cat(
      "Partial bootstrap refinement:",
      sprintf("- Cutoff value: %s", round(object@cutoff, digits = 2)),
      sprintf("- %s to keep: %d of %d (%g%%)", value, keep, total, pc),
      sep = "\n")
  }
)
