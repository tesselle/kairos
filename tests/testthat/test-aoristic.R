

test_that("Aoristic Sum", {
  skip_if_not_installed("folio")
  data("zuni", package = "folio")
  counts <- as_count(zuni)

  ## Set the start and end dates for each ceramic type
  dates <- list(
    LINO = c(600, 875), KIAT = c(850, 950), RED = c(900, 1050),
    GALL = c(1025, 1125), ESC = c(1050, 1150), PUBW = c(1050, 1150),
    RES = c(1000, 1200), TULA = c(1175, 1300), PINE = c(1275, 1350),
    PUBR = c(1000, 1200), WING = c(1100, 1200), WIPO = c(1125, 1225),
    SJ = c(1200, 1300), LSJ = c(1250, 1300), SPR = c(1250, 1300),
    PINER = c(1275, 1325), HESH = c(1275, 1450), KWAK = c(1275, 1450)
  )

  ## Keep only assemblages that have a sample size of at least 50
  keep <- apply(X = zuni, MARGIN = 1, FUN = function(x) sum(x) >= 10)

  ## Calculate date ranges for each assemblage
  span <- apply(
    X = zuni[keep, ],
    FUN = function(x, dates) {
      z <- range(unlist(dates[x > 0]))
      names(z) <- c("from", "to")
      z
    },
    MARGIN = 1,
    dates = dates
  )

  ## Coerce to data.frame
  span <- as.data.frame(t(span))

  ## Calculate aoristic sum (normal)
  aorist_raw <- aoristic(span, step = 25, weight = FALSE)
  gg_raw <- autoplot(aorist_raw)
  vdiffr::expect_doppelganger("aoristic_raw", gg_raw)

  ## Calculate aoristic sum (weights)
  aorist_weigth <- aoristic(span, step = 25, weight = TRUE)
  gg_weight <- autoplot(aorist_weigth)
  vdiffr::expect_doppelganger("aoristic_weight", gg_weight)

  ## Calculate aoristic sum (weights) by group
  groups <- rep(c("A", "B", "C"), times = c(50, 90, 139))
  aorist_groups <- aoristic(span, step = 25, weight = TRUE, groups = groups)
  expect_snapshot(get_weights(aorist_groups))
  for (i in c(TRUE, FALSE)) {
    gg_groups <- autoplot(aorist_groups, facet = i)
    vdiffr::expect_doppelganger(paste0("aoristic_groups_facet-", i), gg_groups)
  }

  expect_error(aoristic(span, groups = groups[1:10]), "must be of length")
  expect_error(aoristic(span, groups = "grp"), "does not have component")
  colnames(span) <- c("A", "B")
  expect_error(aoristic(span), "does not have component")

  ## Rate of change
  roc_groups <- with_seed(12345, roc(aorist_groups))
  for (i in c(TRUE, FALSE)) {
    gg_roc <- autoplot(roc_groups, facet = i)
    vdiffr::expect_doppelganger(paste0("roc_groups_facet-", i), gg_roc)
  }
})
