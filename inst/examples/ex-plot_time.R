data("merzbach", package = "folio")

## Coerce the merzbach dataset to a count matrix
## Keep only decoration types that have a maximum frequency of at least 50
keep <- apply(X = merzbach, MARGIN = 2, FUN = function(x) max(x) >= 50)
counts <- as_count(merzbach[, keep])

## Set dates
## We use the row names as time coordinates (roman numerals)
set_dates(counts) <- as.numeric(utils::as.roman(rownames(counts)))

## Plot abundance vs time
plot_time(counts)
plot_time(counts, facet = TRUE)
