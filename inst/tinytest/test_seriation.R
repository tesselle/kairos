if (requireNamespace("folio", quietly = TRUE)) {
  data("compiegne", package = "folio")
  data("merzbach", package = "folio")

  # Reciprocal Ranking - Counts ================================================
  ## Permute rows
  indices_row <- seriate_rank(compiegne, margin = 1)
  exp_row <- c(1, 2, 5, 3, 4)
  expect_equal(indices_row@rows_order, exp_row)
  expect_equal(indices_row@columns_order, 1:16)

  ## Permute columns
  indices_col <- seriate_rank(compiegne, margin = 2)
  exp_col <- c(14, 1, 11, 3, 16, 12, 5, 2, 15, 13, 4, 7, 6, 9, 10, 8)
  expect_equal(indices_col@rows_order, 1:5)
  expect_equal(indices_col@columns_order, exp_col)
  expect_inherits(get_order(indices_col), "list")

  expect_warning(seriate_rank(compiegne, stop = 1, margin = 2))

  # Reciprocal Ranking on EPPM - Counts ========================================
  indices <- seriate_rank(compiegne, EPPM = TRUE, margin = 2)

  expected <- c("N", "A", "C", "K", "P", "L", "B", "E",
                "I", "M", "D", "G", "O", "J", "F", "H")
  expect_equal(LETTERS[indices@columns_order], expected)

  # Reciprocal Ranking - Incidence =============================================
  incid <- compiegne > 0

  indices <- seriate_rank(incid, margin = c(1, 2))

  exp_row <- c(1, 2, 3, 4, 5)
  exp_col <- c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 14, 15, 13, 16, 9)
  expect_equal(indices@rows_order, exp_row)
  expect_equal(indices@columns_order, exp_col)

  # Average Ranking ============================================================
  indices <- seriate_average(compiegne, margin = c(1, 2))

  exp_row <- c(1, 2, 3, 4, 5)
  exp_col <- c(14, 11, 1, 12, 3, 16, 5, 2, 15, 13, 7, 4, 6, 10, 9, 8)
  expect_equal(indices@rows_order, exp_row)
  expect_equal(indices@columns_order, exp_col)

  expect_error(seriate_average(merzbach), "Empty columns detected.")
}
