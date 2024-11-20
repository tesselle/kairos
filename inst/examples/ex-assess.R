\dontrun{
## Data from Desachy 2004
data("compiegne", package = "folio")

## Correspondance analysis based seriation
(indices <- seriate_average(compiegne, margin = c(1, 2), axes = 1))

## Test significance of seriation results
## Warning: this may take a few seconds!
signif <- assess(indices, axes = 1, n = 1000)

## Histogram of randomized total number of modes
hist(signif$random)

## Observed value is smaller than the 5th percentile of the
## distribution of randomized samples
quantile(signif$random, probs = 0.05)
signif$observed

## Seriation coefficient
## (close to 1: relatively strong and significant signal of unimodality)
signif$coef
}
