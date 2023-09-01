if (requireNamespace("folio", quietly = TRUE)) {
  source("helpers.R")

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

  # Date Model =================================================================
  eve1 <- predict_event(model, margin = 1, calendar = NULL)
  expect_equivalent_to_reference(eve1, file = "_snaps/event_row.rds")

  eve2 <- predict_event(model, margin = 2, calendar = NULL)
  expect_equivalent_to_reference(eve2, file = "_snaps/event_column.rds")

  # Plot =======================================================================
  if (at_home()) {
    using("tinysnapshot")
    options(tinysnapshot_device = "svglite")
    options(tinysnapshot_height = 7) # inches
    options(tinysnapshot_width = 7)
    options(tinysnapshot_tol = 200) # pixels
    options(tinysnapshot_os = "Linux")

    ## Accumulation
    # acc <- predict_accumulation(model)
    # expect_equivalent_to_reference(acc, file = "_snaps/accumulation.rds")

    ## Bootstrap
    # boot <- with_seed(12345, bootstrap(model, n = 30))
    # expect_equivalent_to_reference(boot, file = "_snaps/event_bootstrap.rds")

    ## Jackknife
    jack <- jackknife(model)
    expect_equivalent_to_reference(jack, file = "_snaps/event_jackknife.rds")

    ## Event plot
    for (i in c(TRUE, FALSE)) {
      plot_event_activ <- function() plot(model, type = "activity", event = i,
                                          select = c("LZ1105", "LZ1103"))
      expect_snapshot_plot(plot_event_activ, paste0("plot_event_activ-", i))
    }

    ## Activity plot
    plot_event_tempo <- function() plot(model, type = "tempo",
                                        select = c("LZ1105", "LZ1103"))
    expect_snapshot_plot(plot_event_tempo, "plot_event_tempo")

    ## Errors
    expect_error(plot(model, select = "X"), "Wrong selection")
  }
}
