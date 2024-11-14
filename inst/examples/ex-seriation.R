## Replicates Desachy 2004 results
data("compiegne", package = "folio")

## Get seriation order for columns on EPPM using the reciprocal averaging method
## Expected column order: N, A, C, K, P, L, B, E, I, M, D, G, O, J, F, H
(indices <- seriate_rank(compiegne, EPPM = TRUE, margin = 2))

## Get permutation order
order_rows(indices)
order_columns(indices)

## Permute columns
(new <- permute(compiegne, indices))
