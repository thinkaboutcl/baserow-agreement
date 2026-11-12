test_that("flatten_cell handles Baserow cell formats", {
  expect_equal(flatten_cell(NULL), character(0))
  expect_equal(flatten_cell(list()), character(0))
  expect_equal(flatten_cell(""), character(0))
  expect_equal(flatten_cell("0"), "0")
  expect_equal(flatten_cell(13L), "13")
  # single select
  expect_equal(flatten_cell(list(id = 1, value = "Complete", color = "green")), "Complete")
  # multiple select / link row
  ms <- list(list(id = 1, value = "Epilepsy", color = "a"), list(id = 2, value = "Other", color = "b"))
  expect_equal(flatten_cell(ms), c("Epilepsy", "Other"))
})

test_that("normalise_vec cleans text, maps synonyms, handles unknown", {
  syn <- c("tumour margin" = "tumor margin")
  expect_equal(normalise_vec(c(" Tumour  Margin", "Epilepsy", "epilepsy"), "set", syn),
               c("epilepsy", "tumor margin"))
  expect_equal(normalise_vec(c("Unknown", "Other"), "set"), c("other", "unknown"))
  expect_equal(normalise_vec(c("Unknown", "Other"), "set", unknown = "Unknown"), "other")
  expect_equal(normalise_vec(c("3", "x", ""), "numeric"), 3)
})

test_that("empty_as fills only empty cells", {
  expect_equal(normalise_vec(character(0), "numeric", empty_as = 0), 0)
  expect_equal(normalise_vec("7", "numeric", empty_as = 0), 7)
  expect_equal(normalise_vec(character(0), "numeric"), numeric(0))
  expect_equal(normalise_vec(character(0), "set", empty_as = "None"), "none")
  # Unknown dropped as empty, then filled
  expect_equal(normalise_vec("Unknown", "set", unknown = "Unknown", empty_as = "None"), "none")
  # blank vs 0 now agree
  expect_equal(pair_metrics(normalise_vec(character(0), "numeric", empty_as = 0),
                            normalise_vec("0", "numeric", empty_as = 0))$jaccard, 1)
})
