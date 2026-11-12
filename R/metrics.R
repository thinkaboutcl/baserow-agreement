# Set-based agreement metrics.
#
# For one unit and one field: ref = reference set (A), pred = evaluated set (B).
#   Jaccard   = |A n B| / |A u B|
#   Precision = |A n B| / |B|
#   Recall    = |A n B| / |A|
# A zero denominator returns `empty_both` (NA by default). When exactly one side is
# empty, Jaccard is 0.
#
# Numeric fields may use a tolerance: two numbers "match" when |a - b| <= tolerance.
# Matching is one-to-one, so each value can be matched at most once.

# Which elements of `a` and of `b` are matched to the other side.
match_flags <- function(a, b, tolerance = NULL) {
  if (is.null(tolerance) || !is.numeric(a) || !is.numeric(b)) {
    return(list(a = a %in% b, b = b %in% a))
  }
  fa <- logical(length(a))
  fb <- logical(length(b))
  # Greedy matching on sorted values is optimal for 1-D interval matching.
  ob <- order(b)
  for (i in order(a)) {
    for (j in ob) {
      if (!fb[j] && abs(a[i] - b[j]) <= tolerance + 1e-9) {
        fa[i] <- TRUE
        fb[j] <- TRUE
        break
      }
    }
  }
  list(a = fa, b = fb)
}

pair_metrics <- function(ref, pred, tolerance = NULL, empty_both = NA_real_) {
  ref <- unique(ref)
  pred <- unique(pred)
  n_ref <- length(ref)
  n_pred <- length(pred)
  n_match <- sum(match_flags(ref, pred, tolerance)$a)
  n_union <- n_ref + n_pred - n_match
  list(
    n_ref = n_ref,
    n_pred = n_pred,
    n_match = n_match,
    jaccard = if (n_union == 0) empty_both else n_match / n_union,
    precision = if (n_pred == 0) empty_both else n_match / n_pred,
    recall = if (n_ref == 0) empty_both else n_match / n_ref
  )
}

# Combine several raters' sets into one reference set.
combine_sets <- function(sets, how = c("intersection", "union"), tolerance = NULL) {
  how <- match.arg(how)
  sets <- lapply(sets, unique)
  Reduce(function(a, b) {
    fl <- match_flags(a, b, tolerance)
    if (how == "intersection") a[fl$a] else c(a, b[!fl$b])
  }, sets)
}

# Macro = mean of per-unit scores (units with NA are skipped).
# Micro = pool the counts of all units, then compute once.
aggregate_metrics <- function(n_ref, n_pred, n_match, jaccard, precision, recall) {
  safe_div <- function(x, y) if (y == 0) NA_real_ else x / y
  macro <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  sr <- sum(n_ref)
  sp <- sum(n_pred)
  sm <- sum(n_match)
  list(
    n_units = length(n_ref),
    n_scored = sum(!is.na(jaccard)),
    jaccard_macro = macro(jaccard),
    jaccard_micro = safe_div(sm, sr + sp - sm),
    precision_macro = macro(precision),
    precision_micro = safe_div(sm, sp),
    recall_macro = macro(recall),
    recall_micro = safe_div(sm, sr)
  )
}
