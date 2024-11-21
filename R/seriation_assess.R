#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname assess
#' @aliases assess,AveragePermutationOrder-method
setMethod(
  f = "assess",
  signature = c(object = "AveragePermutationOrder"),
  definition = function(object, axes = 1, n = 1000,
                        progress = getOption("kairos.progress")) {
    ## Validation
    arkhe::assert_length(axes, 1)
    arkhe::assert_length(n, 1)

    ## Reorder along 'axes'
    data <- dimensio::get_data(object)
    perm <- permute(data, object)

    ## Number of local maxima
    freq <- data / rowSums(data)
    a <- sum(apply(X = freq, MARGIN = 2, FUN = local_maxima))

    b <- numeric(n)
    if (n > 0) {
      progress_bar <- interactive() && isTRUE(progress)
      if (progress_bar) pbar <- utils::txtProgressBar(max = n, style = 3)

      for (i in seq_len(n)) {
        ## Randomize original data
        i_obj <- apply(X = data, MARGIN = 2, FUN = sample, replace = FALSE)

        ## Number of local maxima
        i_ca <- seriate_average(i_obj, margin = c(1, 2), axes = axes)
        i_perm <- permute(i_obj, i_ca)
        i_freq <- i_perm / rowSums(i_perm)

        b[[i]] <- sum(apply(X = i_freq, MARGIN = 2, FUN = local_maxima))

        if (progress_bar) utils::setTxtProgressBar(pbar, i)
      }

      if (progress_bar) close(pbar)
    }

    ## Seriation coefficient
    E <- ncol(data)
    M <- nrow(data) * ifelse(E%%2 > 0, E + 1, E) / 2
    S <- (M - a) / (M - E)

    list(
      random = b,
      observed = a,
      expected = E,
      maximum = M,
      coef = S
    )
  }
)

local_maxima <- function(x) {
  n <- length(x)
  left <- c(0, x[-n])
  right <- c(x[-1L], 0)
  sum(x > left & x > right)
}
