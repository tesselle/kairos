test_that("Aoristic sum", {
  ## Exemple from Palmisano et al. 2017
  span <- data.frame(
    from = c(-2200, -2000, -1600, -2000),
    to = c(-1400, -1600, -1400, -1400),
    id = c("a", "b", "c", "d")
  )

  ## Calculate aoristic sum (normal)
  aorist_raw <- aoristic(span, step = 200, weight = FALSE)
  expect_snapshot(aorist_raw)

  gg_raw <- autoplot(aorist_raw)
  vdiffr::expect_doppelganger("aoristic_raw", gg_raw)

  ## Calculate aoristic sum (weights)
  aorist_weigth <- aoristic(span, step = 200, weight = TRUE)
  expect_snapshot(aorist_weigth)

  gg_weight <- autoplot(aorist_weigth)
  vdiffr::expect_doppelganger("aoristic_weight", gg_weight)
})
test_that("Aoristic sum by group", {
  ## Tool data
  span <- data.frame(
    from = c(-1600, -2000, -2200, -1800, -2000, -2200, -2000, -1800, -2000, -2000),
    to = c(-1400, -1400, -1400, -1600, -1400, -1600, -1600, -1600, -1400, -1600)
  )
  groups <- rep(c("A", "B"), 5)

  expect_error(aoristic(span, groups = LETTERS), "must be of length")
  expect_error(aoristic(span, groups = "grp"), "does not have component")

  ## Calculate aoristic sum (weights) by group
  aorist_groups <- aoristic(span, step = 200, weight = TRUE, groups = groups)
  expect_snapshot(aorist_groups)

  aorist_A <- aoristic(span[groups == "A", ], step = 200, weight = TRUE)
  aorist_B <- aoristic(span[groups == "B", ], step = 200, weight = TRUE)
  expect_identical(aorist_groups[, 1], aorist_A[, 1])
  expect_identical(aorist_groups[, 2], aorist_B[, 1])

  for (i in c(TRUE, FALSE)) {
    gg_groups <- autoplot(aorist_groups, facet = i)
    vdiffr::expect_doppelganger(paste0("aoristic_groups_facet-", i), gg_groups)
  }

  ## Rate of change
  roc_groups <- with_seed(12345, roc(aorist_groups))
  for (i in c(TRUE, FALSE)) {
    gg_roc <- autoplot(roc_groups, facet = i)
    vdiffr::expect_doppelganger(paste0("roc_groups_facet-", i), gg_roc)
  }
})
