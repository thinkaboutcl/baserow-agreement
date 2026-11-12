test_that("identical sets score 1", {
  m <- pair_metrics(c("a", "b"), c("b", "a"))
  expect_equal(c(m$jaccard, m$precision, m$recall), c(1, 1, 1))
})

test_that("disjoint sets score 0", {
  m <- pair_metrics(c("a", "b"), c("c"))
  expect_equal(c(m$jaccard, m$precision, m$recall), c(0, 0, 0))
})

test_that("partial overlap", {
  m <- pair_metrics(c("a", "b", "c"), c("b", "c", "d"))
  expect_equal(m$n_match, 2)
  expect_equal(m$jaccard, 2 / 4)
  expect_equal(m$precision, 2 / 3)
  expect_equal(m$recall, 2 / 3)
})

test_that("precision and recall are not symmetric", {
  m <- pair_metrics(c("a", "b", "c", "d"), c("a"))
  expect_equal(m$precision, 1)
  expect_equal(m$recall, 0.25)
})

test_that("both empty follows empty_both", {
  m <- pair_metrics(character(0), character(0))
  expect_true(all(is.na(c(m$jaccard, m$precision, m$recall))))
  m1 <- pair_metrics(character(0), character(0), empty_both = 1)
  expect_equal(c(m1$jaccard, m1$precision, m1$recall), c(1, 1, 1))
})

test_that("one side empty gives Jaccard 0", {
  m <- pair_metrics(character(0), c("a"))
  expect_equal(m$jaccard, 0)
  expect_equal(m$precision, 0)
  expect_true(is.na(m$recall))

  m <- pair_metrics(c("a"), character(0))
  expect_equal(m$jaccard, 0)
  expect_true(is.na(m$precision))
  expect_equal(m$recall, 0)
})

test_that("duplicates are ignored", {
  m <- pair_metrics(c("a", "a", "b"), c("a"))
  expect_equal(m$n_ref, 2)
  expect_equal(m$jaccard, 0.5)
})

test_that("numeric tolerance", {
  expect_equal(pair_metrics(3, 3.5, tolerance = 1)$jaccard, 1)
  expect_equal(pair_metrics(3, 3.5, tolerance = 0)$jaccard, 0)
  expect_equal(pair_metrics(3, 3.5)$jaccard, 0)
  # one-to-one: two close predictions cannot both match one reference value
  m <- pair_metrics(c(10), c(10, 10.5), tolerance = 1)
  expect_equal(m$n_match, 1)
  expect_equal(m$precision, 0.5)
})

test_that("combine_sets intersection and union", {
  s <- list(c("a", "b", "c"), c("b", "c", "d"))
  expect_setequal(combine_sets(s, "intersection"), c("b", "c"))
  expect_setequal(combine_sets(s, "union"), c("a", "b", "c", "d"))
  expect_length(combine_sets(list(character(0), c("a")), "intersection"), 0)
  expect_equal(combine_sets(list(c(5), c(5.4)), "union", tolerance = 0.5), 5)
  expect_equal(sort(combine_sets(list(c(5), c(7)), "union", tolerance = 0.5)), c(5, 7))
})

test_that("macro and micro aggregation differ as expected", {
  u1 <- pair_metrics("a", "a")                    # J = 1
  u2 <- pair_metrics(c("a", "b", "c", "d"), "a")  # J = 0.25
  u3 <- pair_metrics(character(0), character(0))  # NA, skipped by macro
  ms <- list(u1, u2, u3)
  g <- function(k) vapply(ms, function(m) as.numeric(m[[k]]), 0)
  agg <- aggregate_metrics(g("n_ref"), g("n_pred"), g("n_match"), g("jaccard"), g("precision"), g("recall"))
  expect_equal(agg$n_units, 3)
  expect_equal(agg$n_scored, 2)
  expect_equal(agg$jaccard_macro, 0.625)
  expect_equal(agg$jaccard_micro, 2 / 5)
  expect_equal(agg$recall_micro, 2 / 5)
  expect_equal(agg$precision_micro, 1)
})
