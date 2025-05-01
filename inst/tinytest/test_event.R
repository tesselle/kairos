Sys.setenv(LANGUAGE = "en")

if (requireNamespace("folio", quietly = TRUE)) {
  data("zuni", package = "folio")
  zuni_dates <- c(
    LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
    LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
    LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
    LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
  )
  zuni_dates2 <- rep(NA, nrow(zuni))
  zuni_dates2[match(names(zuni_dates), rownames(zuni))] <- zuni_dates

  model <- event(zuni, zuni_dates, rank = 10)
  model2 <- event(zuni, zuni_dates2, rank = 10)
  expect_equal(model, model2)

  ## Date model
  event_summary <- summary(model)
  expect_equivalent_to_reference(event_summary, file = "_snaps/event_summary.rds")

  # coef(model)
  # fitted(model)
  # residuals(model)
  # sigma(model)
  # terms(model)

  eve1 <- predict_event(model, margin = 1, calendar = NULL)
  expect_equivalent_to_reference(eve1, file = "_snaps/event_row.rds")

  eve2 <- predict_event(model, margin = 2, calendar = NULL)
  expect_equivalent_to_reference(eve2, file = "_snaps/event_column.rds")

  acc <- predict_accumulation(model, calendar = NULL)
  expect_equivalent_to_reference(acc, file = "_snaps/accumulation.rds", tolerance = 0.0005)

  # Plot =======================================================================
  if (at_home()) {
    using("tinysnapshot")
    source("helpers.R")

    ## Bootstrap
    # boot <- with_seed(12345, bootstrap(model, n = 30))
    # expect_equivalent_to_reference(boot, file = "_snaps/event_bootstrap.rds")

    ## Jackknife
    jack <- jackknife(model)
    expect_equivalent_to_reference(jack, file = "_snaps/event_jackknife.rds")

    ## Activity plot
    plot_event_activ1 <- function() plot(model, type = "activity", event = FALSE,
                                         select = c("LZ1105", "LZ1103"))
    expect_snapshot_plot(plot_event_activ1, "plot_event_activity1")

    ## Event plot
    plot_event_activ2 <- function() plot(model, type = "activity", event = TRUE,
                                         select = c("LZ1105", "LZ1103"))
    expect_snapshot_plot(plot_event_activ2, "plot_event_activity2")

    ## Tempo plot
    plot_event_tempo <- function() plot(model, type = "tempo",
                                        select = c("LZ1105", "LZ1103"))
    expect_snapshot_plot(plot_event_tempo, "plot_event_tempo")

    ## Errors
    expect_error(plot(model, select = "X"), "Wrong selection")
  }
}
