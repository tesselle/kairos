if (requireNamespace("folio", quietly = TRUE)) {
  data("merzbach", package = "folio")

  ## Coerce the merzbach dataset to a count matrix
  ## Keep only decoration types that have a maximum frequency of at least 50
  keep <- apply(X = merzbach, MARGIN = 2, FUN = function(x) max(x) >= 50)
  counts <- merzbach[, keep]

  ## Group by phase
  ## We use the row names as time coordinates (roman numerals)
  dates <- as.numeric(utils::as.roman(rownames(counts)))

  ## Frequency Increment Test
  freq <- fit(counts, dates, calendar = NULL)
  expect_equal_to_reference(freq, file = "_snaps/fit.rds")

  ## Highlight selection
  roll <- fit(counts, dates, calendar = NULL, roll = TRUE, window = 5)
  expect_equal_to_reference(roll, file = "_snaps/fit_roll.rds")

  # Plot =======================================================================
  if (at_home()) {
    source("helpers.R")
    using("tinysnapshot")
    options(tinysnapshot_device = "svglite")
    options(tinysnapshot_height = 7) # inches
    options(tinysnapshot_width = 7)
    options(tinysnapshot_tol = 200) # pixels
    options(tinysnapshot_os = "Linux")

    ## Plot abundance vs time
    plot_time_abundance <- function() plot_time(counts, dates = dates, n = 3)
    expect_snapshot_plot(plot_time_abundance, "plot_time_abundance")

    ## Plot time vs abundance and highlight selection
    plot_fit <- function() plot(freq, calendar = NULL, ncol = 3, xlab = "Phases")
    expect_snapshot_plot(plot_fit, "plot_fit")

    plot_fit_roll <- function() plot(roll, calendar = NULL, ncol = 3, xlab = "Phases")
    expect_snapshot_plot(plot_fit_roll, "plot_fit_roll")
  }
}
