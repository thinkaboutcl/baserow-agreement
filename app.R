# Web app for baserow-agreement.
# Run locally:   Rscript -e 'shiny::runApp(launch.browser = TRUE)'
# Build the static website (Shinylive, runs fully in the browser):  Rscript build_site.R
#
# Privacy: the Baserow token box is a plain HTML field that Shiny does not see.
# www/baserow.js reads the table in the browser and sends only the table
# content to R. The app never reads BASEROW_TOKEN.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
})
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)


# ---- helpers ---------------------------------------------------------------

split_list <- function(x) {
  x <- trimws(unlist(strsplit(x %||% "", ",")))
  x[nzchar(x)]
}

rater_name <- function(prefix, fallback) {
  n <- gsub("[^A-Za-z0-9]+$", "", prefix)
  if (nzchar(n)) n else fallback
}

# Build the raters part of the config from the prefix boxes.
raters_from_inputs <- function(human_prefixes, model_prefix) {
  hp <- split_list(human_prefixes)
  hn <- if (length(hp)) make.unique(mapply(rater_name, hp, paste0("H", seq_along(hp)), USE.NAMES = FALSE)) else character(0)
  raters <- stats::setNames(as.list(hp), hn)
  model <- NULL
  mp <- trimws(model_prefix %||% "")
  if (nzchar(mp)) {
    model <- make.unique(c(hn, rater_name(mp, "AI")))[length(hn) + 1]
    raters[[model]] <- mp
  }
  list(raters = raters, human_raters = hn, model_rater = model)
}

suffixes_for <- function(meta, prefix) {
  cols <- meta$name[startsWith(meta$name, prefix)]
  substring(cols, nchar(prefix) + 1)
}

# Fields every human has but the model does not: status, notes, row checks...
human_only_fields <- function(meta, r) {
  h <- Reduce(intersect, lapply(r$human_raters, function(x) suffixes_for(meta, r$raters[[x]])))
  if (!is.null(r$model_rater)) h <- setdiff(h, suffixes_for(meta, r$raters[[r$model_rater]]))
  h
}

# Distinct values in the human columns of a field (never the model columns).
human_values <- function(raw, meta, r, field) {
  cols <- intersect(paste0(unlist(r$raters[r$human_raters]), field), names(raw))
  sort(unique(unlist(lapply(cols, function(cn) unlist(raw[[cn]])))))
}

non_rater_columns <- function(meta, raters) {
  meta$name[is.na(vapply(meta$name, rater_of_column, "", raters = raters))]
}

paper_pattern_default <- "Paper\\s*(\\d+)"

# Write the settings that can be uploaded again. Never contains data or the token.
write_settings <- function(cfg, path) {
  out <- list(
    raters = cfg$raters,
    human_raters = cfg$human_raters,
    model_rater = cfg$model_rater,
    fields = lapply(cfg$fields, function(f) f[setdiff(names(f), "split")]),
    text_as = cfg$text_as,
    multi_value_sep = cfg$multi_value_sep,
    units = list(experiment_id = cfg$units$experiment_id,
                 paper_id = cfg$units$paper_id,
                 paper_id_pattern = cfg$units$paper_id_pattern),
    row_filter = cfg$row_filter,
    completion = cfg$completion,
    reference = cfg$reference,
    empty_both = if (is.na(cfg$empty_both)) "NA" else cfg$empty_both,
    unknown_as_empty = cfg$unknown_as_empty,
    unknown_values = cfg$unknown_values
  )
  yaml::write_yaml(out, path)
}

# ---- UI --------------------------------------------------------------------

ui <- page_sidebar(
  title = "Rater agreement",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(includeScript("www/baserow.js")),
  sidebar = sidebar(
    width = 340,
    h5("1. Connect"),
    radioButtons("source_type", NULL, c("Baserow" = "api", "CSV file" = "csv"), inline = TRUE),
    conditionalPanel(
      "input.source_type == 'api'",
      textInput("baserow_url", "Baserow address", "https://api.baserow.io"),
      textInput("table_id", "Table ID", placeholder = "e.g. 123456"),
      helpText("The number after /table/ in the Baserow address bar."),
      div(
        class = "form-group shiny-input-container",
        tags$label(class = "control-label", "Database token"),
        tags$input(type = "password", class = "form-control baserow-token", autocomplete = "off")
      ),
      helpText("Use a read only token. It goes from your browser straight to Baserow and is never saved.")
    ),
    conditionalPanel(
      "input.source_type == 'csv'",
      fileInput("csv_file", "CSV file", accept = ".csv")
    ),
    fileInput("preset_file", "Settings file (optional)", accept = c(".yml", ".yaml"), buttonLabel = "Upload YAML",
              placeholder = "No file chosen"),
    actionButton("connect", "Connect", class = "btn-primary w-100"),
    uiOutput("connect_status")
  ),
  navset_card_underline(
    id = "tabs",
    nav_panel(
      "2. Check columns",
      div(
      p("These are the columns found in your table. Only names and types are shown. No cell values are read here."),
      uiOutput("detected_summary"),
      DT::DTOutput("columns_table")
    )),
    nav_panel(
      "3. Settings",
      div(
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Raters"),
          textInput("human_prefixes", "Human column prefixes (comma separated)", "R1_, R2_"),
          textInput("model_prefix", "AI column prefix (leave empty if none)", "AI_"),
          checkboxInput("text_as_set", "Split text cells like \"a, b\" into lists", FALSE),
          textInput("sep", "List separator", ",", width = "120px")
        ),
        card(
          card_header("Rows and papers"),
          selectInput("paper_col", "Column that tells which paper a row belongs to", NULL),
          textInput("paper_pattern", "Pattern to pull the paper ID out of that column (optional)", ""),
          uiOutput("paper_preview"),
          selectInput("rf_field", "Only use rows where every reviewer chose", NULL),
          selectizeInput("rf_keep", NULL, NULL, multiple = TRUE),
          selectInput("cp_field", "Before round 2, check that every reviewer marked", NULL),
          selectizeInput("cp_done", NULL, NULL, multiple = TRUE)
        )
      ),
      card(
        card_header("Fields to compare"),
        helpText("Tolerance: numbers this close count as equal. Empty as: value used when a cell is empty."),
        uiOutput("fields_ui")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        selectInput("reference", "Right answer for round 2",
                    c("Values all reviewers agree on" = "intersection",
                      "Values any reviewer gave" = "union")),
        selectInput("empty_both", "Score when both sides are empty", c("Skip (NA)" = "NA", "1" = "1")),
        div(
          checkboxInput("unknown_as_empty", "Treat \"Unknown\" as no answer", FALSE),
          fileInput("synonyms_file", "Synonyms CSV (raw, standard)", accept = ".csv"),
          uiOutput("synonyms_status")
        )
      ),
      card(
        card_header("Save these settings"),
        p("Download this page as a settings file. Next time, upload it under ",
          em("Settings file"), " before you click Connect, and this page fills itself in."),
        p("The file holds settings only. It has no data and no token. ",
          "A synonyms CSV is not stored in it, so upload that again each time."),
        downloadButton("download_settings", "Save these settings")
      )
    )),
    nav_panel(
      "4. Results",
      div(
      layout_columns(
        col_widths = c(8, 4),
        radioButtons("round", NULL, c(
          "Round 1: compare the reviewers with each other" = "1",
          "Round 2: also compare with the AI" = "2"
        )),
        div(
          actionButton("run", "Run", class = "btn-primary w-100"),
          br(), br(),
          downloadButton("download", "Download all results", class = "w-100")
        )
      ),
      conditionalPanel(
        "input.round == '2'",
        div(class = "alert alert-warning",
            "Round 2 shows the AI output. Only run it after every reviewer has finished.",
            checkboxInput("force", "Run even if some rows are not marked as finished", FALSE))
      ),
      uiOutput("run_log"),
      h5("Overview"),
      tableOutput("overview"),
      h5("By field"),
      selectInput("level_view", NULL, c("Per paper" = "paper", "Per row" = "experiment"), width = "200px"),
      DT::DTOutput("summary_table"),
      h5("Charts"),
      selectInput("plot_key", NULL, NULL, width = "300px"),
      plotOutput("plot", height = "auto")
    ))
  )
)

# ---- server ----------------------------------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(base = NULL, meta = NULL, raw = NULL, results = NULL, plots = NULL,
                       log = NULL, synonyms = NULL, error = NULL, pending = NULL, busy = FALSE)

  # -- connect ---------------------------------------------------------------
  connect_failed <- function(msg) {
    rv$error <- msg
    rv$busy <- FALSE
    rv$meta <- NULL
    rv$raw <- NULL
  }

  connect_done <- function(cfg, meta, raw) {
    rv$busy <- FALSE
    rv$error <- NULL
    rv$meta <- meta
    rv$raw <- raw
    # A synonyms path inside an uploaded settings file cannot be opened in the
    # browser, so it is dropped here. Upload the synonyms CSV in Settings instead.
    syn <- cfg$synonyms
    rv$synonyms <- if (!is.null(syn) && file.exists(syn)) syn else NULL
    cfg["synonyms"] <- list(rv$synonyms)
    rv$base <- cfg
    fill_settings(cfg)
    nav_select("tabs", "2. Check columns")
  }

  observeEvent(input$connect, {
    rv$results <- NULL
    rv$plots <- NULL
    rv$error <- NULL
    preset <- tryCatch({
      if (!is.null(input$preset_file)) yaml::read_yaml(input$preset_file$datapath) else list()
    }, error = function(e) e)
    if (inherits(preset, "error")) return(connect_failed(paste("Could not read the settings file:", conditionMessage(preset))))
    preset$source <- NULL
    preset$output_dir <- NULL

    if (input$source_type == "api") {
      src <- list(type = "api", baserow_url = trimws(input$baserow_url), table_id = trimws(input$table_id))
      if (!nzchar(src$table_id)) return(connect_failed("Please enter a table ID."))
      cfg <- tryCatch(read_config(NULL, c(preset, list(source = src))), error = function(e) e)
      if (inherits(cfg, "error")) return(connect_failed(conditionMessage(cfg)))
      # The browser reads the table (www/baserow.js) and answers with input$baserow_payload.
      rv$pending <- cfg
      rv$busy <- TRUE
      session$sendCustomMessage("baserow_fetch", list(url = src$baserow_url, table = src$table_id))
    } else {
      if (is.null(input$csv_file)) return(connect_failed("Please choose a CSV file."))
      src <- list(type = "csv", csv_path = input$csv_file$datapath)
      res <- tryCatch({
        cfg <- read_config(NULL, c(preset, list(source = src)))
        list(cfg = cfg, meta = fetch_meta(cfg), raw = suppressMessages(fetch_data(cfg)))
      }, error = function(e) e)
      if (inherits(res, "error")) return(connect_failed(conditionMessage(res)))
      connect_done(res$cfg, res$meta, res$raw)
    }
  })

  observeEvent(input$baserow_payload, {
    req(rv$pending)
    p <- tryCatch(jsonlite::fromJSON(input$baserow_payload, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(p)) return(connect_failed("Could not read the answer from Baserow."))
    if (!isTRUE(p$ok)) {
      msg <- if (identical(p$status, -1L) || identical(p$status, -1)) p$detail else baserow_error_message(p$status, p$detail)
      return(connect_failed(msg))
    }
    res <- tryCatch(list(meta = meta_from_fields(p$fields), raw = rows_to_tibble(p$rows)),
                    error = function(e) e)
    if (inherits(res, "error")) return(connect_failed(conditionMessage(res)))
    if (nrow(res$raw) == 0) return(connect_failed("The table has no rows."))
    connect_done(rv$pending, res$meta, res$raw)
  })

  output$connect_status <- renderUI({
    if (isTRUE(rv$busy)) return(div(class = "alert alert-secondary mt-3", "Reading the table..."))
    if (!is.null(rv$error)) return(div(class = "alert alert-danger mt-3", rv$error))
    if (is.null(rv$meta)) return(NULL)
    div(class = "alert alert-success mt-3",
        sprintf("Connected. %d columns, %d rows.", nrow(rv$meta), nrow(rv$raw)))
  })

  # Fill the settings form from a config (preset or defaults).
  fill_settings <- function(cfg) {
    r <- cfg$raters
    humans <- unlist(r[cfg$human_raters])
    updateTextInput(session, "human_prefixes", value = paste(humans, collapse = ", "))
    updateTextInput(session, "model_prefix", value = if (is.null(cfg$model_rater)) "" else r[[cfg$model_rater]])
    updateCheckboxInput(session, "text_as_set", value = identical(cfg$text_as, "set"))
    updateTextInput(session, "sep", value = cfg$multi_value_sep)
    updateSelectInput(session, "reference", selected = if (cfg$reference %in% c("intersection", "union")) cfg$reference else "intersection")
    updateSelectInput(session, "empty_both", selected = if (is.na(cfg$empty_both)) "NA" else "1")
    updateCheckboxInput(session, "unknown_as_empty", value = isTRUE(cfg$unknown_as_empty))
  }

  raters <- reactive(raters_from_inputs(input$human_prefixes, input$model_prefix))

  # Paper column choices and defaults.
  observe({
    req(rv$meta)
    cols <- non_rater_columns(rv$meta, raters()$raters)
    cfg <- rv$base
    pick <- cfg$units$paper_id
    pattern <- cfg$units$paper_id_pattern %||% ""
    no_paper <- "paper_id" %in% names(cfg$units) && is.null(pick)
    if (no_paper) {
      pick <- ""
    } else if (identical(pick, "auto") || is.null(pick) || !pick %in% cols) {
      links <- rv$meta$name[rv$meta$type == "link_row" & rv$meta$name %in% cols]
      prim <- rv$meta$name[rv$meta$primary]
      first <- if (length(prim) && prim %in% names(rv$raw)) first_or_na(rv$raw[[prim]]) else character(0)
      if (length(first) && mean(grepl(paper_pattern_default, first), na.rm = TRUE) > 0.5) {
        pick <- prim
        pattern <- paper_pattern_default
      } else if (length(links)) {
        pick <- links[[1]]
      } else {
        pick <- ""
      }
    }
    updateSelectInput(session, "paper_col", choices = c("(none: compare rows only)" = "", cols), selected = pick)
    updateTextInput(session, "paper_pattern", value = pattern)
  })

  output$paper_preview <- renderUI({
    req(rv$raw)
    if (!nzchar(input$paper_col %||% "") || !input$paper_col %in% names(rv$raw)) {
      return(helpText("Rows are compared one by one. No per paper results."))
    }
    ids <- first_or_na(rv$raw[[input$paper_col]])
    if (nzchar(input$paper_pattern)) {
      hit <- tryCatch(regmatches(ids, regexec(input$paper_pattern, ids)), error = function(e) NULL)
      if (is.null(hit)) return(helpText(class = "text-danger", "The pattern is not valid."))
      ids <- vapply(hit, function(h) if (length(h) >= 2) h[[2]] else NA_character_, "")
    }
    ok <- unique(ids[!is.na(ids)])
    helpText(sprintf("%d papers found, %d rows without a paper. Examples: %s",
                     length(ok), sum(is.na(ids)), paste(utils::head(ok, 4), collapse = ", ")))
  })

  # Row filter and completion fields.
  observe({
    req(rv$meta)
    h <- human_only_fields(rv$meta, raters())
    cfg <- isolate(rv$base)
    rf <- cfg$row_filter$field %||% ""
    cp <- cfg$completion$field %||% ""
    updateSelectInput(session, "rf_field", choices = c("(no filter: use all rows)" = "", h),
                      selected = if (rf %in% h) rf else "")
    updateSelectInput(session, "cp_field", choices = c("(no check)" = "", h),
                      selected = if (cp %in% h) cp else "")
  })
  observeEvent(list(input$rf_field, rv$raw), {
    req(rv$raw)
    vals <- if (nzchar(input$rf_field %||% "")) human_values(rv$raw, rv$meta, raters(), input$rf_field) else character(0)
    keep <- isolate(rv$base)$row_filter$keep
    sel <- intersect(keep, vals)
    if (!length(sel)) sel <- vals[grepl("^valid", vals, ignore.case = TRUE)]
    updateSelectizeInput(session, "rf_keep", choices = vals, selected = sel)
  })
  observeEvent(list(input$cp_field, rv$raw), {
    req(rv$raw)
    vals <- if (nzchar(input$cp_field %||% "")) human_values(rv$raw, rv$meta, raters(), input$cp_field) else character(0)
    done <- isolate(rv$base)$completion$done
    sel <- intersect(done, vals)
    if (!length(sel)) sel <- vals[grepl("complete|done|finished", vals, ignore.case = TRUE)]
    updateSelectizeInput(session, "cp_done", choices = vals, selected = sel)
  })

  # Config with the current rater, filter and paper settings, fields still "auto".
  partial_cfg <- reactive({
    req(rv$base)
    cfg <- rv$base
    r <- raters()
    cfg$raters <- r$raters
    cfg$human_raters <- r$human_raters
    cfg["model_rater"] <- list(r$model_rater)
    cfg$text_as <- if (isTRUE(input$text_as_set)) "set" else "category"
    cfg$multi_value_sep <- if (nzchar(input$sep %||% "")) input$sep else ","
    cfg["row_filter"] <- list(if (nzchar(input$rf_field %||% ""))
      list(field = input$rf_field, keep = input$rf_keep %||% character(0), compare = TRUE))
    cfg["completion"] <- list(if (nzchar(input$cp_field %||% ""))
      list(field = input$cp_field, done = input$cp_done %||% character(0)))
    cfg$units$paper_id <- if (nzchar(input$paper_col %||% "")) input$paper_col else NULL
    cfg$units$paper_id_pattern <- if (nzchar(input$paper_pattern %||% "")) input$paper_pattern else NULL
    cfg$levels <- c("experiment", "paper")
    cfg
  })

  # Fields detected with the current prefixes (preset field_settings applied).
  detected <- reactive({
    cfg <- partial_cfg()
    cfg$fields <- if (is.list(rv$base$fields)) rv$base$fields else "auto"
    tryCatch(resolve_config(cfg, rv$meta), error = function(e) e)
  })

  output$detected_summary <- renderUI({
    req(rv$meta)
    d <- detected()
    if (inherits(d, "error")) return(div(class = "alert alert-warning", conditionMessage(d)))
    r <- raters()
    div(class = "alert alert-info",
        sprintf("Reviewers: %s. AI: %s. %d fields can be compared: %s.",
                paste(r$human_raters, collapse = ", "), r$model_rater %||% "none",
                length(d$fields), paste(names(d$fields), collapse = ", ")))
  })

  output$columns_table <- DT::renderDT({
    req(rv$meta)
    d <- detected()
    r <- raters()
    role <- vapply(rv$meta$name, function(n) {
      rt <- rater_of_column(n, r$raters)
      if (is.na(rt)) return(if (identical(n, input$paper_col)) "paper" else "")
      f <- substring(n, nchar(r$raters[[rt]]) + 1)
      if (!inherits(d, "error") && !is.null(d$fields[[f]])) return(paste(rt, "- compared as", d$fields[[f]]$type))
      paste(rt, "- not compared")
    }, "")
    DT::datatable(data.frame(Column = rv$meta$name, `Baserow type` = rv$meta$type, Role = role,
                             check.names = FALSE),
                  rownames = FALSE, options = list(pageLength = 50, dom = "ft"))
  })

  # One row of inputs per field.
  output$fields_ui <- renderUI({
    d <- detected()
    if (inherits(d, "error")) return(div(class = "text-danger", conditionMessage(d)))
    fs <- d$fields
    rows <- lapply(seq_along(fs), function(i) {
      f <- fs[[i]]
      layout_columns(
        col_widths = c(4, 3, 2, 3), class = "align-items-center",
        checkboxInput(paste0("fld_inc_", i), names(fs)[i], TRUE),
        selectInput(paste0("fld_type_", i), NULL, c("list (set)" = "set", "single value" = "category",
                                                    "number" = "numeric"), selected = f$type),
        numericInput(paste0("fld_tol_", i), NULL, f$tolerance %||% 0, min = 0),
        textInput(paste0("fld_empty_", i), NULL, as.character(f$empty_as %||% ""), placeholder = "empty as")
      )
    })
    tagList(
      layout_columns(col_widths = c(4, 3, 2, 3), strong("Field"), strong("Type"),
                     strong("Tolerance"), strong("Empty as")),
      rows
    )
  })

  # Build the field inputs even while the Settings tab is hidden.
  outputOptions(output, "fields_ui", suspendWhenHidden = FALSE)

  output$synonyms_status <- renderUI({
    if (!is.null(input$synonyms_file)) return(helpText("Using the uploaded synonyms file."))
    if (!is.null(rv$synonyms)) return(helpText(paste("Using", basename(rv$synonyms), "from the saved settings.")))
    NULL
  })

  # The full config for a run.
  final_cfg <- reactive({
    d <- detected()
    if (inherits(d, "error")) stop(conditionMessage(d), call. = FALSE)
    cfg <- partial_cfg()
    fields <- list()
    for (i in seq_along(d$fields)) {
      # Inputs are NULL until the Settings tab has been opened: then use the defaults.
      if (isFALSE(input[[paste0("fld_inc_", i)]])) next
      f <- list(type = input[[paste0("fld_type_", i)]] %||% d$fields[[i]]$type)
      if (f$type == "numeric") f$tolerance <- input[[paste0("fld_tol_", i)]] %||% d$fields[[i]]$tolerance %||% 0
      ea <- trimws(input[[paste0("fld_empty_", i)]] %||% as.character(d$fields[[i]]$empty_as %||% ""))
      if (nzchar(ea)) f$empty_as <- if (f$type == "numeric") as.numeric(ea) else ea
      fields[[names(d$fields)[i]]] <- f
    }
    if (!length(fields)) stop("Choose at least one field to compare.", call. = FALSE)
    if (!is.null(cfg$row_filter) && !length(cfg$row_filter$keep)) {
      stop("Choose which row values to keep, or turn the row filter off.", call. = FALSE)
    }
    if (!is.null(cfg$completion) && !length(cfg$completion$done)) {
      stop("Choose which values mean finished, or turn the check off.", call. = FALSE)
    }
    cfg$fields <- fields
    cfg$field_settings <- list()
    cfg$reference <- input$reference
    cfg$empty_both <- parse_empty_both(input$empty_both)
    cfg$unknown_as_empty <- isTRUE(input$unknown_as_empty)
    cfg$synonyms <- if (!is.null(input$synonyms_file)) input$synonyms_file$datapath else rv$synonyms
    resolve_config(cfg, rv$meta)
  })

  # -- run ---------------------------------------------------------------------
  observeEvent(input$run, {
    req(rv$raw)
    log <- character(0)
    res <- withProgress(message = "Calculating", tryCatch(
      withCallingHandlers(
        run_analysis(final_cfg(), as.integer(input$round), isTRUE(input$force), raw = rv$raw),
        message = function(m) {
          log <<- c(log, trimws(conditionMessage(m)))
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) e
    ))
    if (inherits(res, "error")) {
      rv$results <- NULL
      rv$plots <- NULL
      rv$log <- list(error = conditionMessage(res), info = log)
      return()
    }
    rv$results <- res
    rv$plots <- make_plots(res$metrics)
    rv$log <- list(error = NULL, info = log)
    updateSelectInput(session, "plot_key", choices = stats::setNames(names(rv$plots), gsub("__", ": ", names(rv$plots))))
  })

  output$run_log <- renderUI({
    l <- rv$log
    if (is.null(l)) return(NULL)
    tagList(
      if (!is.null(l$error)) div(class = "alert alert-danger", l$error),
      if (length(l$info)) helpText(paste(l$info, collapse = " "))
    )
  })

  output$overview <- renderTable({
    req(rv$results)
    s <- rv$results$summary
    rf <- final_cfg()$row_filter$field
    s |>
      filter(field %in% c("(all fields)", rf)) |>
      transmute(Level = ifelse(level == "paper", "Per paper", "Per row"),
                Comparison = gsub("_vs_", " vs ", comparison),
                Field = field, Items = n_units,
                `Jaccard (macro)` = jaccard_macro, `Jaccard (micro)` = jaccard_micro,
                `Precision (micro)` = precision_micro, `Recall (micro)` = recall_micro)
  }, digits = 2, na = "")

  output$summary_table <- DT::renderDT({
    req(rv$results)
    s <- rv$results$summary |>
      filter(level == input$level_view) |>
      select(Comparison = comparison, Field = field, Items = n_units, Scored = n_scored,
             `Jaccard macro` = jaccard_macro, `Jaccard micro` = jaccard_micro,
             `Precision macro` = precision_macro, `Precision micro` = precision_micro,
             `Recall macro` = recall_macro, `Recall micro` = recall_micro)
    if (!rv$results$with_model) s <- select(s, -starts_with("Precision"), -starts_with("Recall"))
    DT::datatable(s, rownames = FALSE, options = list(pageLength = 25, dom = "t")) |>
      DT::formatRound(columns = which(vapply(s, is.double, TRUE)), digits = 2)
  })

  output$plot <- renderPlot({
    req(rv$plots, input$plot_key %in% names(rv$plots))
    rv$plots[[input$plot_key]]
  }, height = function() {
    p <- rv$plots[[input$plot_key %||% ""]]
    if (is.null(p)) 300 else 96 * attr(p, "size")[2]
  })

  output$download_settings <- downloadHandler(
    filename = function() "settings.yml",
    content = function(file) {
      cfg <- tryCatch(final_cfg(), error = function(e) e)
      if (inherits(cfg, "error")) {
        showNotification(conditionMessage(cfg), type = "error")
        req(FALSE)
      }
      write_settings(cfg, file)
    }
  )

  output$download <- downloadHandler(
    filename = function() sprintf("agreement_results_%s.zip", format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(file) {
      req(rv$results)
      dir <- file.path(tempfile("results_"), "results")
      cfg <- final_cfg()
      cfg$output_dir <- dir
      suppressMessages(write_outputs(rv$results$metrics, rv$results$summary, rv$results$rows, cfg))
      write_settings(cfg, file.path(dir, "settings_used.yml"))
      zip::zip(file, files = list.files(dir, recursive = TRUE), root = dir)
    }
  )

  # Make the download links ready even on tabs that are not open yet.
  outputOptions(output, "download", suspendWhenHidden = FALSE)
  outputOptions(output, "download_settings", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
