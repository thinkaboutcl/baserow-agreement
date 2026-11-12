fake_meta <- tibble::tibble(
  name = c("Name", "Paper", "Notes",
           "R1_Colour", "R2_Colour", "AI_Colour",
           "R1_Size", "R2_Size", "AI_Size",
           "R1_Comment", "R2_Comment", "AI_Comment",
           "R1_Status", "R2_Status", "AI_Summary"),
  type = c("text", "link_row", "long_text",
           "multiple_select", "multiple_select", "multiple_select",
           "number", "number", "number",
           "long_text", "long_text", "long_text",
           "single_select", "single_select", "long_text"),
  primary = c(TRUE, rep(FALSE, 14))
)

test_that("fields are detected from columns shared by all raters", {
  cfg <- resolve_config(read_config(NULL, list(source = list(table_id = 1))), fake_meta)
  expect_setequal(names(cfg$fields), c("Colour", "Size"))  # Comment skipped (long_text), Status not for AI
  expect_equal(cfg$fields$Colour$type, "set")
  expect_equal(cfg$fields$Size$type, "numeric")
  expect_equal(cfg$units$paper_id, "Paper")
  expect_equal(cfg$levels, c("experiment", "paper"))
})

test_that("human-only fields are compared when there is no model rater", {
  cfg <- read_config(NULL, list(source = list(table_id = 1), model_rater = NULL))
  cfg <- resolve_config(cfg, fake_meta)
  expect_true("Status" %in% names(cfg$fields))
})

test_that("custom prefixes, exclusions and field_settings", {
  meta <- tibble::tibble(name = c("a.x", "b.x", "m.x", "a.y", "b.y", "m.y"),
                         type = c("number", "number", "number", "text", "text", "text"),
                         primary = FALSE)
  cfg <- read_config(NULL, list(
    source = list(table_id = 1),
    raters = list(A = "a.", B = "b.", M = "m."), human_raters = c("A", "B"), model_rater = "M",
    exclude_fields = "y", field_settings = list(x = list(tolerance = 2, empty_as = 0))
  ))
  cfg <- resolve_config(cfg, meta)
  expect_equal(names(cfg$fields), "x")
  expect_equal(cfg$fields$x$tolerance, 2)
  expect_null(cfg$units$paper_id)
  expect_equal(cfg$levels, "experiment")
})

test_that("text_as = set splits plain text lists, select columns are not split", {
  meta <- tibble::tibble(
    name = c("R1_Species", "R2_Species", "AI_Species", "R1_Tag", "R2_Tag", "AI_Tag", "R1_Days", "R2_Days", "AI_Days"),
    type = c(rep("text", 3), rep("multiple_select", 3), rep("text", 3)),
    primary = FALSE
  )
  cfg <- read_config(NULL, list(source = list(table_id = 1), text_as = "set",
                                field_settings = list(Days = list(type = "numeric"))))
  cfg <- resolve_config(cfg, meta)
  expect_equal(cfg$fields$Species$type, "set")
  expect_true(cfg$fields$Species$split)
  expect_false(cfg$fields$Tag$split)
  expect_equal(cfg$fields$Days$type, "numeric")
  expect_false(cfg$fields$Days$split)

  default <- resolve_config(read_config(NULL, list(source = list(table_id = 1))), meta)
  expect_equal(default$fields$Species$type, "category")
})

test_that("split lists are cleaned and mapped", {
  syn <- c("maize" = "zea mays")
  expect_equal(normalise_vec("Maize,  Hordeum vulgare", "set", syn, split = ","),
               c("hordeum vulgare", "zea mays"))
  expect_equal(normalise_vec(character(0), "set", split = ","), character(0))
})

test_that("clear error when no rater columns match", {
  meta <- tibble::tibble(name = c("foo", "bar"), type = "text", primary = FALSE)
  cfg <- read_config(NULL, list(source = list(table_id = 1)))
  expect_error(resolve_config(cfg, meta), "no field found")
})

test_that("missing table id is reported", {
  expect_error(read_config(NULL), "no table given")
})
