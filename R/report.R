# Summaries (macro + micro), CSV output and plots.

summarise_metrics <- function(metrics, cfg) {
  agg <- function(d) {
    a <- aggregate_metrics(d$n_ref, d$n_pred, d$n_match, d$jaccard, d$precision, d$recall)
    if (all(is.na(d$precision))) {
      a$precision_micro <- NA_real_
      a$recall_micro <- NA_real_
    }
    tibble::as_tibble(a)
  }
  by_field <- metrics |>
    dplyr::group_by(level, comparison, field) |>
    dplyr::group_modify(~ agg(.x)) |>
    dplyr::ungroup()
  # "(all fields)" is the plain average of the field rows, so every field counts
  # the same. The row-status field is reported on its own and left out here.
  mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  all_fields <- by_field |>
    dplyr::filter(field %in% names(cfg$fields)) |>
    dplyr::group_by(level, comparison) |>
    dplyr::summarise(
      n_units = sum(n_units),
      n_scored = sum(n_scored),
      dplyr::across(c(jaccard_macro, jaccard_micro, precision_macro, precision_micro,
                      recall_macro, recall_micro), mean_or_na),
      .groups = "drop"
    ) |>
    dplyr::mutate(field = "(all fields)")
  dplyr::bind_rows(by_field, all_fields) |>
    dplyr::arrange(level, comparison, field == "(all fields)", field)
}

# One ggplot per level x comparison, named "<level>__<comparison>".
# Each plot carries its suggested size (inches) in attr "size".
make_plots <- function(metrics) {
  long <- metrics |>
    tidyr::pivot_longer(c(jaccard, precision, recall), names_to = "metric", values_to = "value") |>
    dplyr::filter(!is.na(value))
  keys <- unique(paste(long$level, long$comparison, sep = "__"))
  plots <- lapply(keys, function(key) {
    d <- long[paste(long$level, long$comparison, sep = "__") == key, ]
    p <- ggplot2::ggplot(d, ggplot2::aes(x = field, y = value)) +
      ggplot2::geom_boxplot(outlier.shape = NA, colour = "grey50") +
      ggplot2::geom_jitter(width = 0.15, height = 0.02, alpha = 0.6, size = 1.5) +
      ggplot2::facet_wrap(~metric) +
      ggplot2::coord_flip(ylim = c(0, 1)) +
      ggplot2::labs(title = gsub("__", ": ", key), x = NULL, y = NULL) +
      ggplot2::theme_bw()
    attr(p, "size") <- c(3 + 3 * length(unique(d$metric)), 1.5 + 0.4 * length(unique(d$field)))
    p
  })
  stats::setNames(plots, keys)
}

plot_metrics <- function(metrics, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  plots <- make_plots(metrics)
  files <- file.path(dir, paste0(names(plots), ".png"))
  for (i in seq_along(plots)) {
    size <- attr(plots[[i]], "size")
    ggplot2::ggsave(files[[i]], plots[[i]], width = size[1], height = size[2], dpi = 150)
  }
  files
}

write_outputs <- function(metrics, summary, rows, cfg) {
  dir <- cfg$output_dir
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(metrics, file.path(dir, "metrics_long.csv"), na = "")
  readr::write_csv(summary, file.path(dir, "summary_by_field.csv"), na = "")
  readr::write_csv(rows, file.path(dir, "rows.csv"), na = "")
  plots <- plot_metrics(metrics, file.path(dir, "plots"))
  message("Wrote ", file.path(dir, c("metrics_long.csv", "summary_by_field.csv", "rows.csv")) |>
    paste(collapse = ", "), " and ", length(plots), " plot(s).")
}
