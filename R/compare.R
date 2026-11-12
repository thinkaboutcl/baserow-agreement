# Round 1: every pair of human raters (Jaccard only).
# Round 2: reference (intersection / union of humans, or a named rater) vs the model rater.

# Build the units for one level from the long table.
# experiment: one unit per kept row; paper: union of each rater's values over a paper's kept rows.
level_data <- function(long, rows, level) {
  d <- long[long$experiment %in% rows$experiment[rows$keep], ]
  if (level == "experiment") {
    d$unit <- d$experiment
  } else {
    d <- d |>
      dplyr::group_by(paper, field, rater) |>
      dplyr::summarise(values = list(unique(unlist(values)) %||% character(0)), .groups = "drop")
    d$unit <- d$paper
  }
  d$level <- level
  d
}

metrics_row <- function(level, paper, unit, field, comparison, m) {
  tibble::tibble(
    level = level, paper = paper, unit = unit, field = field, comparison = comparison,
    n_ref = m$n_ref, n_pred = m$n_pred, n_match = m$n_match,
    jaccard = m$jaccard, precision = m$precision, recall = m$recall
  )
}

human_pairs <- function(cfg) utils::combn(cfg$human_raters, 2, simplify = FALSE)

compare_level <- function(d, cfg, with_model) {
  groups <- split(d, list(d$unit, d$field), drop = TRUE)
  res <- lapply(groups, function(g) {
    sets <- stats::setNames(g$values, g$rater)
    f <- g$field[[1]]
    tol <- field_tolerance(cfg, f)
    out <- list()
    for (p in human_pairs(cfg)) {
      m <- pair_metrics(sets[[p[1]]], sets[[p[2]]], tol, cfg$empty_both)
      m$precision <- NA_real_ # symmetric comparison: only Jaccard is meaningful
      m$recall <- NA_real_
      out[[length(out) + 1]] <- metrics_row(g$level[[1]], g$paper[[1]], g$unit[[1]], f,
                                            paste(p, collapse = "_vs_"), m)
    }
    if (with_model) {
      ref <- if (cfg$reference %in% c("intersection", "union")) {
        combine_sets(sets[cfg$human_raters], cfg$reference, tol)
      } else {
        sets[[cfg$reference]]
      }
      m <- pair_metrics(ref, sets[[cfg$model_rater]], tol, cfg$empty_both)
      out[[length(out) + 1]] <- metrics_row(g$level[[1]], g$paper[[1]], g$unit[[1]], f,
                                            paste0("ref_vs_", cfg$model_rater), m)
    }
    dplyr::bind_rows(out)
  })
  dplyr::bind_rows(res)
}

# Agreement between human raters on the row filter field itself (all rows, unfiltered).
compare_row_status <- function(rows, cfg) {
  if (is.null(cfg$row_filter) || !isTRUE(cfg$row_filter$compare)) return(NULL)
  as_set <- function(x) if (is.na(x)) character(0) else x
  out <- list()
  for (p in human_pairs(cfg)) {
    a <- rows[[paste0("row_status_", p[1])]]
    b <- rows[[paste0("row_status_", p[2])]]
    for (i in seq_len(nrow(rows))) {
      m <- pair_metrics(as_set(a[i]), as_set(b[i]), NULL, cfg$empty_both)
      m$precision <- NA_real_
      m$recall <- NA_real_
      out[[length(out) + 1]] <- metrics_row("experiment", rows$paper[i], rows$experiment[i],
                                            cfg$row_filter$field, paste(p, collapse = "_vs_"), m)
    }
  }
  dplyr::bind_rows(out)
}

# Blinding guard for round 2: every human must have finished every row.
check_completion <- function(rows, cfg) {
  bad <- rep(FALSE, nrow(rows))
  for (h in cfg$human_raters) {
    if (!is.null(cfg$row_filter)) bad <- bad | is.na(rows[[paste0("row_status_", h)]])
    if (!is.null(cfg$completion)) {
      done <- rows[[paste0("completion_", h)]] %in% cfg$completion$done
      bad <- bad | (rows$keep & !done)
    }
  }
  if (any(bad)) {
    stop(sprintf(
      "Round 2 blocked: %d row(s) not finished by all reviewers (row ids: %s). To run anyway, use --force or tick the box in the web app.",
      sum(bad), paste(rows$experiment[bad], collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

run_comparisons <- function(tidy, cfg, with_model) {
  res <- lapply(cfg$levels, function(lv) compare_level(level_data(tidy$long, tidy$rows, lv), cfg, with_model))
  dplyr::bind_rows(c(res, list(compare_row_status(tidy$rows, cfg))))
}
