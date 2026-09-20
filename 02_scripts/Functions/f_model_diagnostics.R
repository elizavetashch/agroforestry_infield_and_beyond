# =============================================================================
# f_model_diagnostics.R — Analysis pipeline: diagnostics and validation
#
# Exported functions:
#   run_gam_diagnostics(model, label)  → named list of diagnostic stats
#   compute_term_variance(model)       → data frame of per-term variance shares
#
# Term labels for m_pfr_int (must match the order of terms in the model
# formula — see fit_pfr_interaction()):
#   1  swf         lf(swf_mat)
#   2  swfD        lf(swf_D)
#   3  distpath    s(distance_to_tree_strip, by=photo_path)
#   4  distnear    ti(distance_to_tree_strip, swf_nearfield)
#   5  disttreeage ti(distance_to_tree_strip, treeage)
#   6  logAFage    log_af_age
#   7  climate     PC1_c
#   8  soil        PC1_s
#   9  shdi        l_shdi
#  10  edge        l_ed
#  11  field       s(field, bs="re")
#  12  year        s(year,  bs="re")
# =============================================================================

PFR_INT_TERM_LABELS <- c(
  "swf", "swfD", "distpath", "distnear", "disttreeage",
  "logAFage", "climate", "soil", "shdi", "edge",
  "field", "year"
)


# ── run_gam_diagnostics ───────────────────────────────────────────────────────

#' Run gam.check() and return its statistics as a named list.
#'
#' Call \code{tar_read(diagnostics_pfr_int, store = "_targets_analysis")} and
#' then \code{print()} to see the full diagnostic summary, or open the saved
#' residual plots.
#'
#' @param model A fitted gam / pfr object.
#' @param label Character string used in the message (e.g. "m_pfr_int").
#' @return Named list: summary text, residual stats, k-index table.
run_gam_diagnostics <- function(model, label = "") {

  message("Running gam.check() for ", label, "...")

  # Capture printed output without opening a graphics device
  txt <- utils::capture.output(
    check_result <- mgcv::gam.check(model, rep = 500)
  )

  list(
    label        = label,
    summary_text = txt,
    k_table      = check_result,
    r_squared    = summary(model)$r.sq,
    dev_expl     = summary(model)$dev.expl,
    n_obs        = stats::nobs(model)
  )
}


# ── compute_term_variance ─────────────────────────────────────────────────────

#' Decompose the fitted value variance by model term.
#'
#' For each term, \code{predict(model, type = "terms")} gives the fitted
#' contribution per observation.  The variance of that vector, divided by the
#' variance of the full fitted values, gives the term's share of the explained
#' variation (terms are not orthogonal so shares do not sum to 1).
#'
#' @param model       A fitted pfr / gam object (typically m_pfr_int).
#' @param term_labels Character vector of human-readable labels in the same
#'   order as the model terms.  Defaults to \code{PFR_INT_TERM_LABELS}.
#' @return Data frame: term_raw, term, variance, proportion — sorted descending.
compute_term_variance <- function(model,
                                  term_labels = PFR_INT_TERM_LABELS) {

  message("Computing per-term variance contributions...")

  term_contrib <- stats::predict(model, type = "terms")
  term_vars    <- apply(term_contrib, 2, stats::var)
  total_var    <- stats::var(stats::fitted(model))

  raw_names <- colnames(term_contrib)

  # Gracefully handle label length mismatches
  if (length(term_labels) != length(raw_names)) {
    warning(
      "term_labels length (", length(term_labels), ") does not match ",
      "number of model terms (", length(raw_names), "). ",
      "Using raw term names instead."
    )
    term_labels <- raw_names
  }

  result <- data.frame(
    term_raw   = raw_names,
    term       = term_labels,
    variance   = as.numeric(term_vars),
    proportion = as.numeric(term_vars / total_var)
  ) |>
    dplyr::arrange(dplyr::desc(proportion))

  message("  Top 3 terms: ",
          paste(result$term[1:3], round(result$proportion[1:3], 3),
                sep = " (", collapse = "), "),
          ")")
  result
}
