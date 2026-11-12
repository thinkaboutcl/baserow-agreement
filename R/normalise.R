# Normalise values before comparison: trim, collapse spaces, lower-case,
# apply synonyms, optionally drop "Unknown"-like values, de-duplicate.
# Numeric fields are converted to numbers.

read_synonyms <- function(path) {
  if (is.null(path)) return(character(0))
  s <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                       show_col_types = FALSE)
  if (!all(c("raw", "standard") %in% names(s))) {
    stop("Synonyms file must have columns 'raw' and 'standard'.", call. = FALSE)
  }
  stats::setNames(clean_text(s$standard), clean_text(s$raw))
}

clean_text <- function(x) tolower(gsub("\\s+", " ", trimws(x)))

# `empty_as`: value used when a rater left the cell empty (per field, optional),
# e.g. 0 for a count where reviewers leave the cell blank to mean none.
# `split`: separator for lists stored as one text value ("a, b"), or NULL.
normalise_vec <- function(v, type, synonyms = character(0), unknown = character(0),
                          empty_as = NULL, split = NULL) {
  if (!is.null(split)) v <- unlist(strsplit(as.character(v), split, fixed = TRUE)) %||% character(0)
  if (type == "numeric") {
    num <- suppressWarnings(as.numeric(v))
    num <- unique(num[!is.na(num)])
    if (length(num) == 0 && !is.null(empty_as)) num <- as.numeric(empty_as)
    return(num)
  }
  v <- clean_text(v)
  hit <- v %in% names(synonyms)
  v[hit] <- synonyms[v[hit]]
  v <- v[nzchar(v) & !v %in% clean_text(unknown)]
  if (length(v) == 0 && !is.null(empty_as)) v <- clean_text(empty_as)
  sort(unique(v))
}

normalise_long <- function(long, cfg) {
  syn <- read_synonyms(cfg$synonyms)
  unknown <- if (isTRUE(cfg$unknown_as_empty)) cfg$unknown_values else character(0)
  types <- vapply(cfg$fields, function(f) f$type, "")
  long$values <- Map(function(v, f) {
    split <- if (isTRUE(cfg$fields[[f]]$split)) cfg$multi_value_sep else NULL
    normalise_vec(v, types[[f]], syn, unknown, cfg$fields[[f]]$empty_as, split)
  }, long$values, long$field)
  long
}
