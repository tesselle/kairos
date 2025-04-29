Sys.setenv(LANGUAGE = "en")

if (at_home() && requireNamespace("SPARTAAS", quietly = TRUE)) {
  data("datacerardat", package = "SPARTAAS")

  res_spartaas <- SPARTAAS::cerardat(
    df = datacerardat$df,
    row.sup = datacerardat$row.sup,
    date = datacerardat$date,
    nf = 9,
    confidence = 0.95,
    graph = FALSE
  )

  res_kairos <- event(
    datacerardat$df,
    dates = datacerardat$date,
    rank = 9,
    sup_row = datacerardat$row.sup,
    calendar = CE()
  )

  pred_spartaas <- res_spartaas$prediction
  pred_event1 <- predict_event(res_kairos, margin = 1, calendar = CE())
  pred_event2 <- predict_event(res_kairos, margin = 2, calendar = CE())
  pred_acc <- predict_accumulation(res_kairos, calendar = CE())

  expect_equal(pred_event1$date, pred_spartaas$Fit_dateEv, tolerance = 0.002)
  expect_equal(pred_event2$date, unname(res_spartaas$date_gt[, "Fit_dateEv"]), tolerance = 0.002)
  expect_equal(pred_acc$date, pred_spartaas$Median_dateAc, tolerance = 0.002)

  expect_equal(round(sigma(res_kairos, calendar = CE()), 2), res_spartaas$statistical_summary$sigma)
  expect_equal(round(summary(res_kairos)$r.squared, 3), res_spartaas$statistical_summary$R2)

  # plot(res_spartaas, which = 1, col1 = rgb(0.93, 0.23, 0.23, 0.5),
  #      col2 = "black", xlim = NULL, ylim = c(0,0.03))
  # plot(res_kairos, select = 1, event = TRUE)
}
