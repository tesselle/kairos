## Data from Husi 2022
data("loire", package = "folio")

## Get time range
loire_range <- loire[, c("lower", "upper")]

## Calculate aoristic sum (normal)
aorist_raw <- aoristic(loire_range, step = 50, weight = FALSE)
plot(aorist_raw, col = "grey")

## Calculate aoristic sum (weights)
aorist_weighted <- aoristic(loire_range, step = 50, weight = TRUE)
plot(aorist_weighted, col = "grey")

## Calculate aoristic sum (weights) by group
aorist_groups <- aoristic(loire_range, step = 50, weight = TRUE,
                          groups = loire$area)
plot(aorist_groups, flip = TRUE, col = "grey")
image(aorist_groups)

## Rate of change
roc_weighted <- roc(aorist_weighted, n = 30)
plot(roc_weighted)

## Rate of change by group
roc_groups <- roc(aorist_groups, n = 30)
plot(roc_groups, flip = TRUE)
