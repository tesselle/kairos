Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8') # Force locale
options(kairos.calendar = calendar("CE"))

if (requireNamespace("folio", quietly = TRUE)) {
  data("zuni", package = "folio")

  ## Set dates
  zuni_dates <- list(
    LINO = c(600, 875), KIAT = c(850, 950), RED = c(900, 1050),
    GALL = c(1025, 1125), ESC = c(1050, 1150), PUBW = c(1050, 1150),
    RES = c(1000, 1200), TULA = c(1175, 1300), PINE = c(1275, 1350),
    PUBR = c(1000, 1200), WING = c(1100, 1200), WIPO = c(1125, 1225),
    SJ = c(1200, 1300), LSJ = c(1250, 1300), SPR = c(1250, 1300),
    PINER = c(1275, 1325), HESH = c(1275, 1450), KWAK = c(1275, 1450)
  )

  ## Calculate mid-point
  zuni_mid_dates <- vapply(X = zuni_dates, FUN = mean, FUN.VALUE = numeric(1))

  ## MCD
  dt <- mcd(zuni, dates = zuni_mid_dates)
  expect_equal_to_reference(as.data.frame(dt), file = "_snaps/mcd.rds")
  expect_error(mcd(zuni, dates = zuni_mid_dates[1:3]))

  # Plot =======================================================================
  if (at_home()) {
    source("helpers.R")
    using("tinysnapshot")
    options(tinysnapshot_device = "svglite")
    options(tinysnapshot_height = 7) # inches
    options(tinysnapshot_width = 7)
    options(tinysnapshot_tol = 200) # pixels
    options(tinysnapshot_os = "Linux")

    ## Bootstrap
    # boot <- with_seed(12345, bootstrap(dt, n = 30))
    # expect_equal_to_reference(boot, file = "_snaps/mcd_bootstrap.rds")

    ## Jackknife
    jack <- jackknife(dt)
    expect_equal_to_reference(jack, file = "_snaps/mcd_jackknife.rds")

    for (i in c(TRUE, FALSE)) {
      plot_mcd <- function() plot(dt[100:125, , ], decreasing = i)
      expect_snapshot_plot(plot_mcd, paste0("mcd_decreasing-", i))
    }
  }
}
