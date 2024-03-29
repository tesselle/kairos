## Replication of Roberts et al. 2012
bayless <- matrix(
  data = c(4, 333, 11, 11, 13, 1605, 252, 9, 48), nrow = 1,
  dimnames = list(c("Bayless"), c("CWW", "CBW", "LMGRW", "LTB", "MMS",
                                  "PBW", "RRW", "SCBW", "TBBW"))
)

## Set ware start and end dates
start <- c(550, 800, 1200, 1150, 1275, 200, 1275, 1200, 750)
end <- c(1325, 1400, 1450, 1300, 1400, 1450, 1450, 1450, 1300)

## Apportion ceramic assemblage under flat/uniform distribution
app <- apportion(bayless, s0 = 1200, s1 = 1350, t0 = start, t1 = end,
                 step = 50, method = "uniform")

## Apportion ceramic assemblage under truncated standard normal distribution
app <- apportion(bayless, s0 = 1200, s1 = 1350, t0 = start, t1 = end,
                 step = 50, method = "truncated", z = 2)

## Array of results
head(app)
