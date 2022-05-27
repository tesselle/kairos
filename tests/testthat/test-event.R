test_that("Date Model", {
  skip_if_not_installed("folio")
  data("zuni", package = "folio")

  ## Set dates
  zuni_dates <- c(
    LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
    LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
    LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
    LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
  )

  model <- event(zuni, zuni_dates, cutoff = 90)

  event <- predict_event(model)
  expect_snapshot(event)

  acc <- predict_accumulation(model)
  expect_snapshot(acc)

  # Errors
  expect_error(event(zuni, zuni_dates, cutoff = 10), "below 50%")
})
test_that("Event Date", {
  skip_if_not_installed("folio")
  data("zuni", package = "folio")

  ## Set dates
  zuni_dates <- c(
    LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
    LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
    LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
    LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
  )

  model <- event(zuni, zuni_dates, cutoff = 90)

  ## Bootstrap
  boot <- with_seed(12345, bootstrap(model, n = 5))
  expect_snapshot(boot)

  ## Jackknife
  jack <- with_seed(12345, jackknife(model))
  expect_snapshot(jack)

  # expect_s4_class(plot(model, select = "LZ1105"), "EventDate")

  skip_if_not_installed("vdiffr")
  # Event plot
  for (i in c(TRUE, FALSE)) {
    gg_date_act <- autoplot(model, type = "activity", event = i,
                            select = c("LZ1105", "LZ1103"))
    vdiffr::expect_doppelganger(paste0("date_activity_event-", i), gg_date_act)
  }

  # Activity plot
  gg_date_tempo <- autoplot(model, type = "tempo",
                            select = c("LZ1105", "LZ1103"))
  vdiffr::expect_doppelganger("date_tempo", gg_date_tempo)

  # Errors
  expect_error(autoplot(model, select = "X"), "Wrong selection")
})
