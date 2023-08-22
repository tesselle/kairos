## Data from Crema et al. 2016
data("merzbach", package = "folio")

## Coerce the merzbach dataset to a count matrix
## Keep only decoration types that have a maximum frequency of at least 50
keep <- apply(X = merzbach, MARGIN = 2, FUN = function(x) max(x) >= 50)
counts <- merzbach[, keep]

## Set dates
## We use the row names as time coordinates (roman numerals)
dates <- as.numeric(utils::as.roman(rownames(counts)))

## Plot abundance vs time
plot_time(counts, dates, calendar = NULL, ncol = 3, xlab = "Phases")
