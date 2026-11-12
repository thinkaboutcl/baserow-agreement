# Configuration: built-in defaults < optional YAML file < command-line overrides.
# With the defaults, only a table ID (or a CSV path) is needed: fields and their
# types are detected from the table itself (see R/schema.R).

config_defaults <- function() {
  list(
    source = list(type = "api", baserow_url = "https://api.baserow.io", table_id = NULL, csv_path = NULL),
    units = list(experiment_id = "id", paper_id = "auto", paper_id_pattern = NULL),
    levels = c("experiment", "paper"),
    raters = list(R1 = "R1_", R2 = "R2_", AI = "AI_"),
    human_raters = c("R1", "R2"),
    model_rater = "AI",
    fields = "auto",
    field_settings = list(),
    exclude_fields = character(0),
    row_filter = NULL,
    completion = NULL,
    multi_value_sep = ",",
    text_as = "category",
    reference = "intersection",
    empty_both = NA,
    unknown_as_empty = FALSE,
    unknown_values = "Unknown",
    synonyms = NULL,
    output_dir = "output"
  )
}

# `path` may be NULL (no file). `overrides` uses the same structure as the YAML.
read_config <- function(path = NULL, overrides = list()) {
  cfg <- config_defaults()
  if (!is.null(path)) {
    if (!file.exists(path)) stop("Config file not found: ", path, call. = FALSE)
    cfg <- merge_config(cfg, yaml::read_yaml(path))
  }
  cfg <- merge_config(cfg, overrides)
  cfg$empty_both <- parse_empty_both(cfg$empty_both)
  cfg$levels <- unlist(cfg$levels)
  cfg$human_raters <- unlist(cfg$human_raters)
  cfg$unknown_values <- unlist(cfg$unknown_values)
  cfg$exclude_fields <- unlist(cfg$exclude_fields)
  if (!is.null(cfg$row_filter)) cfg$row_filter$keep <- unlist(cfg$row_filter$keep)
  if (!is.null(cfg$completion)) cfg$completion$done <- unlist(cfg$completion$done)
  validate_source(cfg)
  cfg
}

# Top-level keys are replaced, except `source` and `units`, which are merged key by key.
merge_config <- function(base, new) {
  for (k in names(new)) {
    if (k %in% c("source", "units") && is.list(new[[k]])) {
      for (kk in names(new[[k]])) base[[k]][kk] <- list(new[[k]][[kk]])
    } else {
      base[k] <- list(new[[k]])
    }
  }
  base
}

parse_empty_both <- function(x) {
  if (is.null(x) || (length(x) == 1 && (is.na(x) || identical(toupper(as.character(x)), "NA")))) {
    return(NA_real_)
  }
  x <- as.numeric(x)
  if (is.na(x) || !x %in% c(0, 1)) stop("empty_both must be NA, 0 or 1.", call. = FALSE)
  x
}

config_error <- function(...) stop("Config error: ", ..., call. = FALSE)

validate_source <- function(cfg) {
  if (!cfg$source$type %in% c("csv", "api")) config_error("source$type must be 'csv' or 'api'.")
  if (!cfg$text_as %in% c("category", "set")) config_error("text_as must be 'category' or 'set'.")
  if (cfg$source$type == "api" && is.null(cfg$source$table_id)) {
    config_error("no table given. Use --table <id> or set source$table_id.")
  }
  if (cfg$source$type == "csv" && is.null(cfg$source$csv_path)) {
    config_error("no CSV given. Use --csv <file> or set source$csv_path.")
  }
  invisible(TRUE)
}

# Run after fields have been resolved against the table.
validate_config <- function(cfg) {
  err <- config_error
  if (!all(cfg$levels %in% c("experiment", "paper"))) err("levels must be experiment and/or paper.")
  if (length(cfg$raters) == 0 || is.null(names(cfg$raters))) err("raters must map names to column prefixes.")
  if (length(cfg$human_raters) < 2) err("human_raters needs at least two raters.")
  if (!all(cfg$human_raters %in% names(cfg$raters))) err("every human_raters entry must be in raters.")
  if (!is.null(cfg$model_rater) && !cfg$model_rater %in% names(cfg$raters)) err("model_rater must be in raters.")

  if (length(cfg$fields) == 0) err("no fields to compare.")
  types <- vapply(cfg$fields, function(f) f$type, "")
  bad <- types[!types %in% c("set", "category", "numeric")]
  if (length(bad)) err("unknown field type(s): ", paste(names(bad), bad, sep = "=", collapse = ", "))
  for (f in names(cfg$fields)) {
    ea <- cfg$fields[[f]]$empty_as
    if (is.null(ea)) next
    if (length(ea) != 1) err("fields$", f, "$empty_as must be a single value.")
    if (types[[f]] == "numeric" && is.na(suppressWarnings(as.numeric(ea)))) {
      err("fields$", f, "$empty_as must be a number for a numeric field.")
    }
  }

  if (!cfg$reference %in% c("intersection", "union", names(cfg$raters))) {
    err("reference must be intersection, union or a rater name.")
  }
  if (!is.null(cfg$synonyms) && !file.exists(cfg$synonyms)) err("synonyms file not found: ", cfg$synonyms)
  invisible(TRUE)
}

field_tolerance <- function(cfg, field) {
  f <- cfg$fields[[field]]
  if (is.null(f) || f$type != "numeric") NULL else f$tolerance
}
