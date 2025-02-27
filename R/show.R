# SHOW METHODS
#' @include AllClasses.R
NULL

# EventDate ====================================================================
setMethod(
  f = "show",
  signature = "EventDate",
  definition = function(object) {
    show_ca <- utils::capture.output(methods::callNextMethod(object))
    summary_lm <- summary(object)
    sig <- sigma(object, calendar = get_calendar())

    cat(
      show_ca,
      "",
      paste0(tr_("Multiple Linear Regression"), ":"),
      sprintf(tr_("* R-squared: %s"), round(summary_lm$r.squared, 3)),
      sprintf(tr_("* Adjusted R-squared: %s"), round(summary_lm$adj.r.squared, 3)),
      sprintf(tr_("* Residual standard error: %s"), round(sig, 2)),
      sep = "\n"
    )
  }
)

# PermutationOrder =============================================================
setMethod(
  f = "show",
  signature = "PermutationOrder",
  definition = function(object) {
    k <- 50
    rows <- strtrim(paste0(object@rows_order, collapse = " "), k)
    columns <- strtrim(paste0(object@columns_order, collapse = " "), k)
    cat(
      paste0(tr_("Permutation order for matrix seriation"), ":"),
      sprintf(tr_("* Row order: %s"), paste0(rows, "...")),
      sprintf(tr_("* Column order: %s"), paste0(columns, "...")),
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
      paste0("Partial bootstrap refinement", ":"),
      sprintf("* Cutoff value: %s", round(object@cutoff, digits = 2)),
      sprintf("* %s to keep: %d of %d (%g%%)", value, keep, total, pc),
      sep = "\n")
  }
)
