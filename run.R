#!/usr/bin/env Rscript
# Usage:
#   Rscript run.R --table <id> [options]        any Baserow table, fields detected automatically
#   Rscript run.R --csv <file> [options]        a CSV export
#   Rscript run.R [config.yml] [options]        settings from a config file
#
# Options:
#   --inspect        show the detected columns/fields and stop (reads no cell values)
#   --round 1|2      1 (default): human vs human only. 2: also reference vs model.
#                    Run round 2 ONLY after all human raters have finished.
#   --force          skip the "all reviewers finished" check in round 2
#   --url <url>      Baserow server (default https://api.baserow.io)
#   --config <file>  config file (combined with --table / --csv if both are given)
#   --out <dir>      output folder (default output)
#
# The token is read from the BASEROW_TOKEN environment variable (.Renviron).

suppressPackageStartupMessages(library(dplyr))

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
for (env in unique(c(".Renviron", file.path(root, ".Renviron")))) {
  if (!nzchar(Sys.getenv("BASEROW_TOKEN")) && file.exists(env)) readRenviron(env)
}

# ---- arguments ----
args <- commandArgs(trailingOnly = TRUE)
with_value <- c("--table", "--csv", "--url", "--config", "--out", "--round")
opt <- list()
positional <- character(0)
i <- 1
while (i <= length(args)) {
  a <- args[i]
  if (a %in% with_value) {
    if (i == length(args)) stop(a, " needs a value.", call. = FALSE)
    opt[[a]] <- args[i + 1]
    i <- i + 2
  } else if (startsWith(a, "--")) {
    if (!a %in% c("--inspect", "--force")) stop("Unknown option: ", a, call. = FALSE)
    opt[[a]] <- TRUE
    i <- i + 1
  } else {
    positional <- c(positional, a)
    i <- i + 1
  }
}
round <- as.integer(opt[["--round"]] %||% 1)
if (!round %in% c(1, 2)) stop("--round must be 1 or 2.", call. = FALSE)

config_path <- opt[["--config"]] %||% (if (length(positional)) positional[[1]])
if (is.null(config_path) && is.null(opt[["--table"]]) && is.null(opt[["--csv"]])) {
  if (file.exists("config.yml")) {
    config_path <- "config.yml"
  } else {
    stop("Nothing to analyse. Use --table <id>, --csv <file> or a config file. See README.", call. = FALSE)
  }
}

overrides <- list()
if (!is.null(opt[["--table"]])) overrides$source <- list(type = "api", table_id = opt[["--table"]])
if (!is.null(opt[["--csv"]])) overrides$source <- list(type = "csv", csv_path = opt[["--csv"]])
if (!is.null(opt[["--url"]])) overrides$source$baserow_url <- opt[["--url"]]
if (!is.null(opt[["--out"]])) overrides$output_dir <- opt[["--out"]]

# ---- configuration + structure ----
cfg <- read_config(config_path, overrides)
if (!is.null(config_path)) message("Using config: ", config_path)
meta <- fetch_meta(cfg)
cfg <- resolve_config(cfg, meta)

if (isTRUE(opt[["--inspect"]])) {
  print_inspect(meta, cfg)
  quit(save = "no")
}

with_model <- round == 2 && !is.null(cfg$model_rater)
message(sprintf("Round %d: %s | %d field(s): %s", round,
                if (with_model) "human pairs + reference vs model" else "human pairs only (model output not used)",
                length(cfg$fields), paste(names(cfg$fields), collapse = ", ")))
if (with_model && is.null(cfg$completion)) {
  message("WARNING: no completion check configured. Make sure every reviewer has finished before reading these results.")
}

# ---- analysis ----
res <- run_analysis(cfg, round, force = isTRUE(opt[["--force"]]))
write_outputs(res$metrics, res$summary, res$rows, cfg)

print(as.data.frame(
  res$summary |>
    filter(field %in% c("(all fields)", cfg$row_filter$field)) |>
    select(level, comparison, field, n_units, jaccard_macro, jaccard_micro,
           precision_micro, recall_micro) |>
    mutate(across(where(is.double), ~ round(.x, 3)))
))
