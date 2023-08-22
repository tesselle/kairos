## Data from Crema et al. 2016
data("merzbach", package = "folio")

## Keep only decoration types that have a maximum frequency of at least 50
keep <- apply(X = merzbach, MARGIN = 2, FUN = function(x) max(x) >= 50)
counts <- merzbach[, keep]

## Group by phase
## We use the row names as time coordinates (roman numerals)
dates <- as.numeric(utils::as.roman(rownames(counts)))

## Frequency Increment Test
freq <- fit(counts, dates, calendar = NULL)

## Plot time vs abundance
plot(freq, calendar = NULL, ncol = 3, xlab = "Phases")

## Plot time vs abundance and highlight selection
freq <- fit(counts, dates, calendar = NULL, roll = TRUE, window = 5)
plot(freq, calendar = NULL, ncol = 3, xlab = "Phases")
