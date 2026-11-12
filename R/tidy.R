# Turn the raw table into
#   rows: one line per experiment (row) with its paper id, row status per human rater,
#         completion status per human rater, and whether the row is kept
#   long: experiment x paper x field x rater, with a list-column `values`

first_or_na <- function(x) vapply(x, function(v) if (length(v)) v[[1]] else NA_character_, "")

rater_col <- function(cfg, rater, field) {
  as.vector(outer(unlist(cfg$raters[rater]), field, paste0))
}

tidy_data <- function(raw, cfg, raters = names(cfg$raters)) {
  if (!cfg$units$experiment_id %in% names(raw)) {
    message("Row id column '", cfg$units$experiment_id, "' not found: using row numbers.")
    raw[[cfg$units$experiment_id]] <- as.list(as.character(seq_len(nrow(raw))))
  }
  if (is.null(cfg$units$paper_id)) {
    raw[[".paper"]] <- raw[[cfg$units$experiment_id]]
    cfg$units$paper_id <- ".paper"
  }
  needed <- c(cfg$units$experiment_id, cfg$units$paper_id)
  for (r in raters) needed <- c(needed, rater_col(cfg, r, names(cfg$fields)))
  if (!is.null(cfg$row_filter)) needed <- c(needed, rater_col(cfg, cfg$human_raters, cfg$row_filter$field))
  if (!is.null(cfg$completion)) needed <- c(needed, rater_col(cfg, cfg$human_raters, cfg$completion$field))
  missing <- setdiff(needed, names(raw))
  if (length(missing)) stop("Columns not found in data: ", paste(missing, collapse = ", "), call. = FALSE)

  experiment <- first_or_na(raw[[cfg$units$experiment_id]])
  paper <- first_or_na(raw[[cfg$units$paper_id]])
  if (!is.null(cfg$units$paper_id_pattern)) {
    hit <- regmatches(paper, regexec(cfg$units$paper_id_pattern, paper))
    paper <- vapply(hit, function(h) if (length(h) >= 2) h[[2]] else NA_character_, "")
  }

  rows <- tibble::tibble(experiment = experiment, paper = paper)
  for (h in cfg$human_raters) {
    if (!is.null(cfg$row_filter)) {
      rows[[paste0("row_status_", h)]] <- first_or_na(raw[[rater_col(cfg, h, cfg$row_filter$field)]])
    }
    if (!is.null(cfg$completion)) {
      rows[[paste0("completion_", h)]] <- first_or_na(raw[[rater_col(cfg, h, cfg$completion$field)]])
    }
  }

  dropped <- is.na(rows$paper) | is.na(rows$experiment)
  if (any(dropped)) message(sprintf("Dropped %d row(s) without a paper id.", sum(dropped)))

  rows$keep <- TRUE
  if (!is.null(cfg$row_filter)) {
    st <- as.matrix(rows[paste0("row_status_", cfg$human_raters)])
    rows$keep <- apply(st, 1, function(s) all(!is.na(s) & s %in% cfg$row_filter$keep))
  }
  raw <- raw[!dropped, ]
  rows <- rows[!dropped, ]
  message(sprintf("%d rows%s%s.", nrow(rows),
                  if (is.null(cfg$row_filter)) "" else sprintf(", %d kept by row filter", sum(rows$keep)),
                  if (cfg$units$paper_id == ".paper") "" else sprintf(", %d papers", length(unique(rows$paper)))))

  long <- list()
  for (r in raters) {
    for (f in names(cfg$fields)) {
      long[[length(long) + 1]] <- tibble::tibble(
        experiment = rows$experiment, paper = rows$paper,
        field = f, rater = r, values = raw[[rater_col(cfg, r, f)]]
      )
    }
  }
  list(rows = rows, long = dplyr::bind_rows(long), raw = raw)
}
