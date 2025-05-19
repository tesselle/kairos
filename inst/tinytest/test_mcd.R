Sys.setenv(LANGUAGE = "en")

zuni <- data.frame(
  LINO = c(0, 0, 0, 0, 0, 0, 0, 30, 0, 0, 2, 120),
  KIAT = c(0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 24),
  RED = c(0, 0, 0, 0, 0, 0, 1, 5, 0, 0, 0, 11),
  GALL = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 12),
  ESC = c(0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 25),
  PUBW = c(1, 2, 0, 5, 0, 4, 1, 0, 0, 1, 3, 35),
  RES = c(5, 11, 2, 1, 1, 7, 12, 0, 8, 1, 1, 42),
  TULA = c(440, 222, 161, 63, 115, 9, 2, 0, 4, 3, 40, 18),
  PINE = c(18, 25, 27, 8, 6, 0, 0, 0, 0, 0, 0, 0),
  PUBR = c(0, 2, 0, 0, 0, 3, 2, 0, 3, 2, 0, 3),
  WING = c(8, 22, 2, 5, 6, 8, 10, 0, 13, 13, 0, 9),
  WIPO = c(2, 0, 0, 1, 0, 3, 3, 0, 7, 4, 0, 0),
  SJ = c(1022, 957, 678, 234, 225, 1, 6, 0, 1, 13, 1, 0),
  LSJ = c(163, 128, 172, 38, 25, 0, 0, 0, 0, 0, 0, 0),
  SPR = c(2, 3, 6, 10, 0, 0, 0, 0, 0, 0, 0, 0),
  PINER = c(4, 1, 9, 6, 0, 0, 0, 0, 0, 0, 0, 0),
  HESH = c(4, 12, 28, 3, 5, 0, 0, 0, 0, 0, 0, 0),
  KWAK = c(0, 16, 62, 2, 1, 0, 0, 0, 0, 0, 0, 0),
  row.names = c("CS11", "CS12", "CS144", "CS195", "CS40", "LZ0219",
                "LZ0280", "LZ0367", "LZ0508", "LZ0560", "LZ1076", "LZ1087")
)

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
  using("tinysnapshot")
  source("helpers.R")

  if (Sys.info()["sysname"] != "Darwin") {
    ## Bootstrap
    boot <- suppressWarnings(bootstrap(dt, n = 30, seed = 12345))
    expect_equal_to_reference(boot, file = "_snaps/mcd_bootstrap.rds")
  }

  ## Jackknife
  jack <- jackknife(dt)
  expect_equal_to_reference(jack, file = "_snaps/mcd_jackknife.rds")

  for (i in c(TRUE, FALSE)) {
    plot_mcd <- function() plot(dt, decreasing = i)
    expect_snapshot_plot(plot_mcd, paste0("mcd_decreasing-", i))
  }
}
