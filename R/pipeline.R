# The whole analysis as one function. Used by run.R and by the web app (app.R).

# Raters whose columns are read in a round. Round 1 never reads the model columns.
raters_for_round <- function(cfg, with_model) {
  raters <- cfg$human_raters
  if (with_model) raters <- c(raters, cfg$model_rater)
  if (!cfg$reference %in% c("intersection", "union")) raters <- union(raters, cfg$reference)
  raters
}

# cfg must already be resolved (resolve_config). `raw` can be passed in to avoid
# fetching the table again.
run_analysis <- function(cfg, round = 1, force = FALSE, raw = NULL) {
  with_model <- round == 2 && !is.null(cfg$model_rater)
  if (is.null(raw)) raw <- fetch_data(cfg)
  tidy <- tidy_data(raw, cfg, raters_for_round(cfg, with_model))
  if (with_model && !force) check_completion(tidy$rows, cfg)
  tidy$long <- normalise_long(tidy$long, cfg)
  metrics <- run_comparisons(tidy, cfg, with_model)
  list(
    metrics = metrics,
    summary = summarise_metrics(metrics, cfg),
    rows = tidy$rows,
    with_model = with_model
  )
}
