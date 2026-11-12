# Detect the table structure so that no column names need to be written by hand.
#
# A field is compared when a column "<prefix><field>" exists for EVERY rater in use
# (e.g. R1_Pathology, R2_Pathology, AI_Pathology). Columns that only some raters have
# (notes, status, reviewer name, ...) are ignored automatically.

# Baserow field type -> comparison type (NULL = not comparable, skipped).
baserow_type_map <- c(
  multiple_select = "set", link_row = "set", multiple_collaborators = "set",
  single_select = "category", text = "category", boolean = "category",
  email = "category", url = "category", phone_number = "category", date = "category",
  number = "numeric", rating = "numeric", duration = "numeric",
  csv_text = "set"                   # non-numeric CSV column, may hold "a, b" lists
)

# Column metadata: tibble(name, type, link) where type is the Baserow type
# (or a guessed one for CSV) and link is TRUE for link-to-table columns.
fetch_meta <- function(cfg) {
  if (cfg$source$type == "api") fetch_baserow_meta(cfg) else csv_meta(cfg)
}

fetch_baserow_meta <- function(cfg) {
  meta_from_fields(baserow_get(cfg, sprintf("api/database/fields/table/%s/", cfg$source$table_id)))
}

# `fields` is the parsed JSON list returned by Baserow's "list fields" endpoint.
meta_from_fields <- function(fields) {
  tibble::tibble(
    name = vapply(fields, function(f) f$name, ""),
    type = vapply(fields, function(f) f$type, ""),
    primary = vapply(fields, function(f) isTRUE(f$primary), TRUE)
  )
}

csv_meta <- function(cfg) {
  raw <- readr::read_csv(cfg$source$csv_path,
    col_types = readr::cols(.default = readr::col_character()),
    na = character(), show_col_types = FALSE
  )
  guess <- vapply(raw, function(x) {
    x <- trimws(x[nzchar(trimws(x))])
    if (length(x) == 0) return("text")
    if (all(!is.na(suppressWarnings(as.numeric(x))))) "number" else "csv_text"
  }, "")
  tibble::tibble(name = names(raw), type = unname(guess), primary = seq_along(guess) == 1)
}

rater_of_column <- function(name, raters) {
  hit <- names(raters)[vapply(raters, function(p) startsWith(name, p), TRUE)]
  if (length(hit)) hit[[which.max(nchar(unlist(raters[hit])))]] else NA_character_
}

# Fill in `fields` (when "auto") and `units$paper_id` (when "auto"), then validate.
resolve_config <- function(cfg, meta) {
  used <- c(cfg$human_raters, cfg$model_rater)
  if (!cfg$reference %in% c("intersection", "union")) used <- c(used, cfg$reference)
  used <- unique(used)

  if (identical(cfg$fields, "auto")) {
    per_rater <- lapply(used, function(r) {
      p <- cfg$raters[[r]]
      cols <- meta$name[startsWith(meta$name, p)]
      stats::setNames(meta$type[match(cols, meta$name)], substring(cols, nchar(p) + 1))
    })
    common <- Reduce(intersect, lapply(per_rater, names))
    common <- setdiff(common, cfg$exclude_fields)
    if (!is.null(cfg$row_filter)) common <- setdiff(common, cfg$row_filter$field)
    if (!is.null(cfg$completion)) common <- setdiff(common, cfg$completion$field)
    fields <- list()
    skipped <- character(0)
    for (f in common) {
      t <- per_rater[[1]][[f]]
      ctype <- if (t == "text") cfg$text_as else baserow_type_map[t]
      if (is.na(ctype)) {
        skipped <- c(skipped, sprintf("%s (%s)", f, t))
        next
      }
      fields[[f]] <- list(type = unname(ctype))
    }
    if (length(skipped)) message("Skipped fields with non-comparable types: ", paste(skipped, collapse = ", "))
    if (length(fields) == 0) {
      config_error(sprintf(
        "no field found with a column for every rater (%s). Check the prefixes under `raters`, or run with --inspect.",
        paste(sprintf("%s = '%s'", used, unlist(cfg$raters[used])), collapse = ", ")
      ))
    }
    cfg$fields <- fields
  } else {
    for (f in names(cfg$fields)) if (is.null(cfg$fields[[f]]$type)) cfg$fields[[f]]$type <- "set"
  }
  # Per-field options layered on top (works with auto and explicit fields).
  for (f in names(cfg$field_settings)) {
    if (is.null(cfg$fields[[f]])) {
      warning("field_settings: '", f, "' is not a compared field; ignored.", call. = FALSE)
      next
    }
    cfg$fields[[f]] <- utils::modifyList(cfg$fields[[f]], cfg$field_settings[[f]])
  }
  # A set stored in a plain text column ("a, b, c") is split on multi_value_sep.
  # Select and link columns already hold separate values, so they are not split.
  first_rater <- cfg$raters[[used[[1]]]]
  for (f in names(cfg$fields)) {
    if (!is.null(cfg$fields[[f]]$split)) next
    btype <- meta$type[match(paste0(first_rater, f), meta$name)]
    cfg$fields[[f]]$split <- cfg$fields[[f]]$type == "set" && isTRUE(btype %in% c("text", "long_text", "csv_text"))
  }

  if (identical(cfg$units$paper_id, "auto")) {
    no_rater <- is.na(vapply(meta$name, rater_of_column, "", raters = cfg$raters))
    links <- meta$name[meta$type == "link_row" & no_rater]
    cfg$units$paper_id <- if (length(links)) links[[1]] else NULL
  }
  if (is.null(cfg$units$paper_id) && "paper" %in% cfg$levels) {
    message("No paper column found: comparing row by row only (set units$paper_id to enable paper level).")
    cfg$levels <- setdiff(cfg$levels, "paper")
  }

  validate_config(cfg)
  cfg
}

# Human-readable overview of what was detected. Shows column names and types only,
# never cell values, so it is safe to run before reviewers have finished.
print_inspect <- function(meta, cfg) {
  role <- vapply(meta$name, function(n) {
    r <- rater_of_column(n, cfg$raters)
    if (is.na(r)) {
      if (identical(n, cfg$units$paper_id)) return("paper id")
      if (identical(n, cfg$units$experiment_id)) return("row id")
      return("")
    }
    f <- substring(n, nchar(cfg$raters[[r]]) + 1)
    if (!is.null(cfg$fields[[f]])) {
      split <- if (isTRUE(cfg$fields[[f]]$split)) sprintf(", split on '%s'", cfg$multi_value_sep) else ""
      return(sprintf("%s -> compared as %s%s", r, cfg$fields[[f]]$type, split))
    }
    if (!is.null(cfg$row_filter) && f == cfg$row_filter$field) return(sprintf("%s -> row filter", r))
    if (!is.null(cfg$completion) && f == cfg$completion$field) return(sprintf("%s -> completion check", r))
    sprintf("%s -> ignored", r)
  }, "")
  cat("\nColumns found:\n")
  print(as.data.frame(tibble::tibble(column = meta$name, baserow_type = meta$type, role = role)),
        row.names = FALSE, right = FALSE)
  cat("\nRaters:", paste(sprintf("%s ('%s')", names(cfg$raters), unlist(cfg$raters)), collapse = ", "),
      "| humans:", paste(cfg$human_raters, collapse = ", "), "| model:", cfg$model_rater %||% "none", "\n")
  cat("Fields compared (", length(cfg$fields), "): ",
      paste(sprintf("%s [%s]", names(cfg$fields), vapply(cfg$fields, `[[`, "", "type")), collapse = ", "),
      "\n", sep = "")
  cat("Row id:", cfg$units$experiment_id, "| paper id:", cfg$units$paper_id %||% "(none)",
      "| levels:", paste(cfg$levels, collapse = ", "), "\n")
  cat("Row filter:", if (is.null(cfg$row_filter)) "none (all rows compared)" else
        sprintf("%s in {%s}", cfg$row_filter$field, paste(cfg$row_filter$keep, collapse = ", ")), "\n")
  cat("Completion check for round 2:", if (is.null(cfg$completion)) "none" else
        sprintf("%s in {%s}", cfg$completion$field, paste(cfg$completion$done, collapse = ", ")), "\n\n")
}
