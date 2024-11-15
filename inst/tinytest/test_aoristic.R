Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8') # Force locale
options(kairos.calendar = calendar("CE"))

# Aoristic sum =================================================================
## Exemple from Palmisano et al. 2017
span <- data.frame(
  from = c(-2200, -2000, -1600, -2000),
  to = c(-1400, -1600, -1400, -1400),
  id = c("a", "b", "c", "d")
)

## Calculate aoristic sum (normal)
aorist_raw <- aoristic(span, step = 200, weight = FALSE)
expect_equal_to_reference(aorist_raw, file = "_snaps/aoristic_raw.rds")
expect_equal_to_reference(as.data.frame(aorist_raw), file = "_snaps/aoristic.rds")

expect_equal(weights(aorist_raw), aorist_raw@p)
expect_equal(length(span(aorist_raw)), 4L)

## Calculate aoristic sum (weights)
aorist_weight <- aoristic(span, step = 200, weight = TRUE)
expect_equal_to_reference(aorist_weight, file = "_snaps/aoristic_weight.rds")

# Aoristic sum by group ========================================================
## Tool data
span <- data.frame(
  from = c(-1600, -2000, -2200, -1800, -2000, -2200, -2000, -1800, -2000, -2000),
  to = c(-1400, -1400, -1400, -1600, -1400, -1600, -1600, -1600, -1400, -1600)
)
groups <- rep(c("A", "B"), 5)

expect_error(aoristic(span, groups = LETTERS), "must be of length")
expect_error(aoristic(span, groups = "grp"), "does not have component")

## Calculate aoristic sum (weights) by group
aorist_group <- aoristic(span, step = 200, weight = TRUE, groups = groups)
expect_equal_to_reference(aorist_group, file = "_snaps/aoristic_group.rds")

expect_equal(dim(weights(aorist_group)), c(10L, 4L, 2L))

aorist_A <- aoristic(span[groups == "A", ], step = 200, weight = TRUE)
aorist_B <- aoristic(span[groups == "B", ], step = 200, weight = TRUE)
expect_identical(aorist_group[, 1, , drop = TRUE], aorist_A[, 1, , drop = TRUE])
expect_identical(aorist_group[, 2, , drop = TRUE], aorist_B[, 1, , drop = TRUE])

# Plot =========================================================================
if (at_home()) {
  source("helpers.R")
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  plot_aoristic_raw <- function() plot(aorist_raw, col = "grey")
  expect_snapshot_plot(plot_aoristic_raw, "plot_aoristic_raw")

  plot_aoristic_weight <- function() plot(aorist_weight, col = "grey")
  expect_snapshot_plot(plot_aoristic_weight, "plot_aoristic_weight")

  plot_aoristic_group <- function() plot(aorist_group, col = "grey")
  expect_snapshot_plot(plot_aoristic_group, "plot_aoristic_group")

  ## Rate of change
  roc_group <- with_seed(12345, roc(aorist_group))
  plot_roc_group <- function() plot(roc_group, col = "grey")
  expect_snapshot_plot(plot_roc_group, "plot_roc_group")
}
