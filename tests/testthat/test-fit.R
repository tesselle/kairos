test_that("FIT", {
  skip_if_not_installed("folio")
  data("merzbach", package = "folio")

  ## Coerce the merzbach dataset to a count matrix
  ## Keep only decoration types that have a maximum frequency of at least 50
  keep <- apply(X = merzbach, MARGIN = 2, FUN = function(x) max(x) >= 50)
  counts <- as_count(merzbach[, keep])

  ## Group by phase
  ## We use the row names as time coordinates (roman numerals)
  set_dates(counts) <- as.numeric(utils::as.roman(rownames(counts)))

  ## Plot abundance vs time
  for (i in c(TRUE, FALSE)) {
    gg_time <- plot_time(counts, facet = i)
    vdiffr::expect_doppelganger(paste0("time_facet-", i), gg_time)
  }

  ## Frequency Increment Test
  freq <- fit(counts)

  ## Plot time vs abundance and highlight selection
  gg_fit <- autoplot(freq)
  vdiffr::expect_doppelganger("time_fit", gg_fit)

  gg_roll <- autoplot(freq, roll = TRUE, window = 5)
  vdiffr::expect_doppelganger("time_roll", gg_roll)
})
