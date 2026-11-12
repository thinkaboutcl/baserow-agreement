test_that("(all fields) is the plain average of the field scores", {
  cfg <- list(fields = list(A = list(type = "set"), B = list(type = "set")))
  row <- function(unit, field, m) {
    tibble::tibble(level = "experiment", paper = "1", unit = unit, field = field, comparison = "R1_vs_R2",
                   n_ref = m$n_ref, n_pred = m$n_pred, n_match = m$n_match,
                   jaccard = m$jaccard, precision = NA_real_, recall = NA_real_)
  }
  metrics <- dplyr::bind_rows(
    row("1", "A", pair_metrics("x", "x")),               # A: 1
    row("2", "A", pair_metrics("x", "y")),               # A: 0   -> A macro 0.5
    row("1", "B", pair_metrics("x", "x")),               # B: 1   -> B macro 1
    row("2", "B", pair_metrics(character(0), character(0))),  # skipped
    row("1", "Row Status", pair_metrics("v", "w"))       # not a field: left out
  )
  s <- summarise_metrics(metrics, cfg)
  all <- s[s$field == "(all fields)", ]
  expect_equal(nrow(all), 1)
  expect_equal(all$jaccard_macro, (0.5 + 1) / 2)
  expect_equal(all$n_units, 4)
  expect_equal(all$n_scored, 3)
  expect_true(is.na(all$precision_macro))
})
