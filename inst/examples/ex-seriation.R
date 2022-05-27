## Replicates Desachy 2004 results
## Coerce dataset to abundance matrix
data("compiegne", package = "folio")

## Get seriation order for columns on EPPM using the reciprocal averaging method
## Expected column order: N, A, C, K, P, L, B, E, I, M, D, G, O, J, F, H
(indices <- seriate_rank(compiegne, EPPM = TRUE, margin = 2))

## Permute columns
(new <- permute(compiegne, indices))

## See the vignette
\dontrun{
utils::vignette("seriation")
}
