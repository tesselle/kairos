## Data from Peeples and Schachner 2012
data("zuni", package = "folio")

## Assume that some assemblages are reliably dated (this is NOT a real example)
zuni_dates <- c(
  LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
  LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
  LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
  LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
)

## Model the event and accumulation date for each assemblage
model <- event(zuni, zuni_dates, rank = 10)
plot(model, select = 1:10, event = TRUE, flip = TRUE)
