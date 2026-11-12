# Read the raw table from a CSV export or from the Baserow REST API (GET only).
# Both return a tibble with one list-column per table column; every cell is a
# character vector (length 0 = empty, length > 1 = multiple values).

fetch_data <- function(cfg) {
  switch(cfg$source$type,
    csv = fetch_csv(cfg),
    api = fetch_baserow(cfg)
  )
}

# Command line access to Baserow. Always GET: nothing is ever written back.
# The token comes from the BASEROW_TOKEN environment variable.
# (The web app does not use this. It reads Baserow from the browser, see www/baserow.js.)
baserow_get <- function(cfg, path, query = list()) {
  token <- Sys.getenv("BASEROW_TOKEN")
  if (!nzchar(token)) {
    stop("BASEROW_TOKEN is not set. Put your read-only database token in a .Renviron file ",
         "in the project folder (see README).", call. = FALSE)
  }
  resp <- httr2::request(cfg$source$baserow_url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_headers(Authorization = paste("Token", trimws(token))) |>
    httr2::req_method("GET") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    detail <- tryCatch(httr2::resp_body_json(resp)$detail, error = function(e) "")
    stop(baserow_error_message(status, detail), call. = FALSE)
  }
  httr2::resp_body_json(resp)
}

# A readable error for a failed Baserow request. Status 0 means no answer at all.
baserow_error_message <- function(status, detail = "") {
  hint <- switch(as.character(status),
    "0" = "Could not reach Baserow. Check the Baserow address and your internet connection.",
    "401" = "Check the token. It may be wrong, deleted, or not allowed to read this database.",
    "403" = "The token has no read permission on this database.",
    "404" = "Table not found. Check the table ID. It is the number after /table/ in the Baserow URL.",
    ""
  )
  if (status == 0) return(hint)
  trimws(sprintf("Baserow returned HTTP %d: %s %s", status, detail %||% "", hint))
}

fetch_baserow <- function(cfg) {
  path <- sprintf("api/database/rows/table/%s/", cfg$source$table_id)
  rows <- list()
  page <- 1
  repeat {
    body <- baserow_get(cfg, path, list(user_field_names = "true", size = 200, page = page))
    rows <- c(rows, body$results)
    if (is.null(body[["next"]])) break
    page <- page + 1
  }
  message(sprintf("Fetched %d rows from Baserow table %s.", length(rows), cfg$source$table_id))
  rows_to_tibble(rows)
}

# Flatten one Baserow cell to a character vector.
# Handles scalars, single select {id, value, color}, multiple select / link rows
# [{id, value, ...}, ...] and nulls.
flatten_cell <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  if (is.atomic(x)) {
    x <- as.character(x)
    return(x[!is.na(x) & nzchar(trimws(x))])
  }
  if (!is.null(names(x)) && "value" %in% names(x)) return(flatten_cell(x[["value"]]))
  unlist(lapply(x, flatten_cell), use.names = FALSE) %||% character(0)
}

rows_to_tibble <- function(rows) {
  cols <- unique(unlist(lapply(rows, names)))
  out <- lapply(cols, function(cn) lapply(rows, function(r) flatten_cell(r[[cn]])))
  names(out) <- cols
  tibble::as_tibble(out)
}

fetch_csv <- function(cfg) {
  raw <- readr::read_csv(cfg$source$csv_path,
    col_types = readr::cols(.default = readr::col_character()),
    na = character(), show_col_types = FALSE
  )
  # Multi-value cells are split later, in normalise_long(), using the field settings.
  out <- lapply(names(raw), function(cn) lapply(raw[[cn]], function(v) flatten_cell(trimws(v))))
  names(out) <- names(raw)
  message(sprintf("Read %d rows from %s.", nrow(raw), cfg$source$csv_path))
  tibble::as_tibble(out)
}
