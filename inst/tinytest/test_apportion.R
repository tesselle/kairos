Sys.setenv(LANGUAGE = "en")

# Chronological Apportioning ===================================================
## Replication of Roberts et al. 2012
bayless <- matrix(data = c(4, 333, 11, 11, 13, 1605, 252, 9, 48), nrow = 1)
rownames(bayless) <- c("Bayless")
colnames(bayless) <- c("CWW", "CBW", "LMGRW", "LTB", "MMS",
                       "PBW", "RRW", "SCBW", "TBBW")

## Set ware start and end dates
start <- c(550, 800, 1200, 1150, 1275, 200, 1275, 1200, 750)
end <- c(1325, 1400, 1450, 1300, 1400, 1450, 1450, 1450, 1300)

## Apportion ceramic assemblage under flat/uniform distribution
app_uni <- apportion(bayless, s0 = 1200, s1 = 1350, t0 = start, t1 = end,
                     step = 50, method = "uniform")
expect_equal_to_reference(app_uni, file = "_snaps/apportion_uni.rds")

## Apportion ceramic assemblage under truncated standard normal distribution
app_trunc <- apportion(bayless, s0 = 1200, s1 = 1350, t0 = start, t1 = end,
                       step = 50, method = "truncated", z = 2)
expect_equal_to_reference(app_trunc, file = "_snaps/apportion_trunc.rds")
