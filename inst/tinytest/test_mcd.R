Sys.setenv(LANGUAGE = "en")

zuni <- matrix(
  data = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 30L, 0L, 0L, 2L, 120L,
           0L, 0L, 0L, 3L, 0L, 0L, 0L, 3L, 0L, 0L, 0L, 24L, 0L, 0L, 0L,
           0L, 0L, 0L, 1L, 5L, 0L, 0L, 0L, 11L, 0L, 0L, 0L, 0L, 0L, 0L,
           0L, 0L, 1L, 0L, 0L, 12L, 0L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 0L,
           1L, 0L, 25L, 1L, 2L, 0L, 5L, 0L, 4L, 1L, 0L, 0L, 1L, 3L, 35L,
           5L, 11L, 2L, 1L, 1L, 7L, 12L, 0L, 8L, 1L, 1L, 42L, 440L, 222L,
           161L, 63L, 115L, 9L, 2L, 0L, 4L, 3L, 40L, 18L, 18L, 25L, 27L,
           8L, 6L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 0L, 0L, 0L, 3L, 2L,
           0L, 3L, 2L, 0L, 3L, 8L, 22L, 2L, 5L, 6L, 8L, 10L, 0L, 13L, 13L,
           0L, 9L, 2L, 0L, 0L, 1L, 0L, 3L, 3L, 0L, 7L, 4L, 0L, 0L, 1022L,
           957L, 678L, 234L, 225L, 1L, 6L, 0L, 1L, 13L, 1L, 0L, 163L, 128L,
           172L, 38L, 25L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 3L, 6L, 10L,
           0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 4L, 1L, 9L, 6L, 0L, 0L, 0L, 0L,
           0L, 0L, 0L, 0L, 4L, 12L, 28L, 3L, 5L, 0L, 0L, 0L, 0L, 0L, 0L,
           0L, 0L, 16L, 62L, 2L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
  nrow = 12, ncol = 18,
  dimnames = list(
    c("CS11", "CS12", "CS144", "CS195", "CS40", "LZ0219", "LZ0280",
      "LZ0367", "LZ0508", "LZ0560", "LZ1076", "LZ1087"),
    c("LINO", "KIAT", "RED", "GALL", "ESC", "PUBW", "RES", "TULA", "PINE",
      "PUBR", "WING", "WIPO", "SJ", "LSJ", "SPR", "PINER", "HESH", "KWAK")
  )
)

dates <- c(LINO = 737.5, KIAT = 900, RED = 975, GALL = 1075, ESC = 1100,
           PUBW = 1100, RES = 1100, TULA = 1237.5, PINE = 1312.5, PUBR = 1100,
           WING = 1150, WIPO = 1175, SJ = 1250, LSJ = 1275, SPR = 1275,
           PINER = 1300, HESH = 1362.5, KWAK = 1362.5)


## MCD
dt <- mcd(zuni, dates = dates)
expect_equal_to_reference(as.data.frame(dt), file = "_snaps/mcd.rds")
expect_error(mcd(zuni, dates = dates[1:3]))

## Jackknife
jack <- jackknife(dt)
expect_equal_to_reference(jack, file = "_snaps/mcd_jackknife.rds")

if (at_home() && Sys.info()["sysname"] != "Darwin") {
  using("tinysnapshot")
  source("helpers.R")

  ## Bootstrap
  boot <- suppressWarnings(bootstrap(dt, n = 30, seed = 12345))
  expect_equal_to_reference(boot, file = "_snaps/mcd_bootstrap.rds")

  ## Plot
  for (i in c(TRUE, FALSE)) {
    plot_mcd <- function() plot(dt, decreasing = i)
    expect_snapshot_plot(plot_mcd, paste0("mcd_decreasing-", i))
  }
}
