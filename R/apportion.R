# CHRONOLOGICAL APPORTIONING
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname apportion
#' @aliases apportion,data.frame-method
setMethod(
  f = "apportion",
  signature = c(object = "data.frame"),
  definition = function(object, s0, s1, t0, t1, from = min(s0), to = max(s1),
                        step = 25, method = c("uniform", "truncated"), z = 2,
                        progress = getOption("kairos.progress")) {
    object <- data.matrix(object)
    methods::callGeneric(object, s0, s1, t0, t1, from = from, to = to,
                         step = step, method = method, z = z,
                         progress = progress)
  }
)

#' @export
#' @rdname apportion
#' @aliases apportion,matrix-method
setMethod(
  f = "apportion",
  signature = c(object = "matrix"),
  definition = function(object, s0, s1, t0, t1, from = min(s0), to = max(s1),
                        step = 25, method = c("uniform", "truncated"), z = 2,
                        progress = getOption("kairos.progress")) {
    ## Validation
    method <- match.arg(method, several.ok = FALSE)
    arkhe::assert_lower(s0, s1, strict = FALSE)
    arkhe::assert_lower(t0, t1, strict = FALSE)
    arkhe::assert_lower(from, to, strict = TRUE)

    ## Get data
    n_site <- nrow(object)
    n_type <- ncol(object)
    span <- to - from

    ## Number of periods (rounded toward the smallest integer)
    n_periode <- ceiling(span / step)
    t <- seq_len(n_periode)
    periode <- paste(from + (t - 1) * step, from + t * step, sep = "_")

    ## Empty array to store apportioning probabilities
    p <- array(data = NA_real_, dim = c(nrow(object), ncol(object), n_periode))
    dimnames(p) <- c(dimnames(object), list(periode))
    a <- p

    ## Round the site dates to be multiples of the step size
    ## (rounded toward the smallest/largest multiple)
    s0[s0 %% step != 0] <- floor(s0[s0 %% step != 0] / step) * step
    s1[s1 %% step != 0] <- ceiling(s1[s1 %% step != 0] / step) * step

    ## Type midpoints and life spans
    m <- (t1 + t0) / 2
    g <- t1 - t0

    ## Distribution function
    fun <- switch (
      method,
      uniform = dist_uniform,
      truncated = dist_truncated
    )

    ## Apportion
    k_site <- seq_len(n_site)
    k_type <- seq_len(n_type)

    progress_bar <- interactive() && isTRUE(progress)
    if (progress_bar) pbar <- utils::txtProgressBar(max = n_site, style = 3)

    for (i in k_site) {
      for (j in k_type) {
        ## If the type lies outside the known site occupation or study interval
        if (t1[j] <= s0[i] | t0[j] >= s1[i]) next # Do not apportion

        ## Earliest and latest overlaps between the ware and site
        vj0 <- max(s0[i], t0[j])
        vj1 <- min(s1[i], t1[j])

        ## Get the apportioning probability for any period
        qjt0 <- vapply(X = s0[i] + (t - 1) * step, FUN = max,
                       FUN.VALUE = numeric(1), t0[j])
        qjt1 <- vapply(X = s0[i] + t * step, FUN = min,
                       FUN.VALUE = numeric(1), t1[j])

        p[i, j, ] <- fun(vj0, vj1, qjt0, qjt1, g[j], m[j], z)
      }

      if (progress_bar) utils::setTxtProgressBar(pbar, i)
    }

    if (progress_bar) close(pbar)

    ## Remove negative values
    p[p < 0] <- 0

    ## Apportion
    a[] <- apply(X = p, MARGIN = 3, FUN = function(x, counts) x * counts,
                 counts = object)

    .CountApportion(
      a,
      p = p,
      method = method,
      from = from,
      to = to,
      step = step
    )
  }
)

dist_uniform <- function(v_j0, v_j1, q_jt0, q_jt1, ...) {
  (q_jt1 - q_jt0) / (v_j1 - v_j0)
}
dist_truncated <- function(v_j0, v_j1, q_jt0, q_jt1, g, m, z) {
  z_j0 <- (v_j0 - m) / (g / (2 * z))
  z_j1 <- (v_j1 - m) / (g / (2 * z))

  z_jt0 <- (q_jt0 - m) / (g / (2 * z))
  z_jt1 <- (q_jt1 - m) / (g / (2 * z))

  (phi(z_jt1, z) - phi(z_jt0, z)) / (phi(z_j1, z) - phi(z_j0, z))
}
phi <- function(x, z) {
  extraDistr::ptnorm(x, mean = 0, sd = 1, a = -z, b = z)
}
