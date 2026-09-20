# =============================================================================
# f_models.R — Analysis pipeline: model fitting
#
# Exported functions:
#   fit_pfr_interaction(mod_data, swf_inputs) → m_pfr_int
#   fit_pfr_base(mod_data, swf_inputs)        → m_pfr
#   fit_gam_full(mod_data)                    → m_gam_full
#   compare_aic(m_pfr_int, m_pfr, m_gam_full) → data frame
#
# Note on scoping: pfr() evaluates lf(swf_mat, …) and lf(swf_D, …) in the
# function's local environment, where swf_mat / swf_D are created from
# swf_inputs.  This mirrors the global-environment behaviour of the original
# script.
# =============================================================================


# ── fit_pfr_interaction ───────────────────────────────────────────────────────

#' Fit the full PFR model with baseline SWF functional effect and its
#' distance-interaction functional effect.
#'
#' Terms:
#'   lf(swf_mat)                           — β₀(r): baseline SWF coefficient function
#'   lf(swf_D)                             — β₁(r): modulation by tree-strip distance
#'   s(distance_to_tree_strip, by=photo_path) — C3/C4 distance smooth
#'   ti(distance_to_tree_strip, swf_nearfield) — near-field SWF × distance interaction
#'   ti(distance_to_tree_strip, treeage)    — tree age × distance interaction
#'   log_af_age                             — log AF system age (linear)
#'   PC1_c, PC1_s                           — climate and soil PC scores
#'   l_shdi, l_ed                           — landscape diversity / edge density
#'   s(field, bs="re"), s(year, bs="re")    — random effects
#'
#' @param mod_data   Data frame from \code{add_pca_scores()}.
#' @param swf_inputs List from \code{build_swf_inputs()}.
#' @return A fitted \code{pfr} / \code{gam} object.
fit_pfr_interaction <- function(mod_data, swf_inputs) {

  message("Fitting m_pfr_int (PFR with SWF × distance interaction)...")

  # Unpack into the local environment so pfr() formula can find them
  swf_mat  <- swf_inputs$mat
  swf_D    <- swf_inputs$swf_D
  argvals  <- swf_inputs$argvals

  model <- refund::pfr(
    yield_tha ~
      refund::lf(swf_mat, argvals = argvals, k = 3) +
      refund::lf(swf_D,   argvals = argvals, k = 3) +
      mgcv::s(distance_to_tree_strip, by = photo_path, k = 3) +
      mgcv::ti(distance_to_tree_strip, swf_nearfield, k = c(3, 3)) +
      mgcv::ti(distance_to_tree_strip, treeage,       k = c(3, 3)) +
      log_af_age +
      PC1_c +
      PC1_s +
      l_shdi +
      l_ed +
      mgcv::s(field, bs = "re") +
      mgcv::s(year,  bs = "re"),
    data   = mod_data,
    method = "REML"
  )

  message("  m_pfr_int — R² = ", round(summary(model)$r.sq, 3),
          " | AIC = ", round(AIC(model), 1))
  model
}


# ── fit_pfr_base ──────────────────────────────────────────────────────────────

#' Fit the simpler PFR model without the SWF × distance interaction term.
#' Nested within m_pfr_int; use AIC comparison to evaluate the interaction.
#'
#' @param mod_data   Data frame from \code{add_pca_scores()}.
#' @param swf_inputs List from \code{build_swf_inputs()}.
#' @return A fitted \code{pfr} / \code{gam} object.
fit_pfr_base <- function(mod_data, swf_inputs) {

  message("Fitting m_pfr (PFR baseline, no SWF × distance interaction)...")

  swf_mat <- swf_inputs$mat
  argvals <- swf_inputs$argvals

  model <- refund::pfr(
    yield_tha ~
      refund::lf(swf_mat, argvals = argvals, k = 3) +
      mgcv::s(distance_to_tree_strip, by = photo_path, k = 3) +
      mgcv::ti(distance_to_tree_strip, swf_nearfield, k = c(3, 3)) +
      mgcv::ti(distance_to_tree_strip, treeage,       k = c(3, 3)) +
      log_af_age +
      PC1_c +
      PC1_s +
      l_shdi +
      l_ed +
      mgcv::s(field, bs = "re") +
      mgcv::s(year,  bs = "re"),
    data   = mod_data,
    method = "REML"
  )

  message("  m_pfr — R² = ", round(summary(model)$r.sq, 3),
          " | AIC = ", round(AIC(model), 1))
  model
}


# ── fit_gam_full ──────────────────────────────────────────────────────────────

#' Fit the GAM reference model (no functional terms).
#' Used as a benchmark to show what the functional terms add.
#'
#' @param mod_data Data frame from \code{add_pca_scores()}.
#' @return A fitted \code{gam} object.
fit_gam_full <- function(mod_data) {

  message("Fitting m_gam_full (GAM reference, no functional terms)...")

  model <- mgcv::gam(
    yield_tha ~
      mgcv::s(distance_to_tree_strip, by = photo_path, k = 3) +
      mgcv::ti(distance_to_tree_strip, swf_slope,     k = c(3, 3)) +
      mgcv::ti(distance_to_tree_strip, swf_nearfield, k = c(3, 3)) +
      af_age +
      mgcv::ti(distance_to_tree_strip, treeage, k = c(3, 3)) +
      PC1_c + PC1_s +
      l_shdi + l_ed +
      mgcv::s(field, bs = "re") +
      mgcv::s(year,  bs = "re"),
    data   = mod_data,
    family = stats::gaussian(),
    method = "REML"
  )

  message("  m_gam_full — R² = ", round(summary(model)$r.sq, 3),
          " | AIC = ", round(AIC(model), 1))
  model
}


# ── compare_aic ───────────────────────────────────────────────────────────────

#' Compute an AIC comparison table for all three models.
#'
#' @param m_pfr_int Fitted model from \code{fit_pfr_interaction()}.
#' @param m_pfr     Fitted model from \code{fit_pfr_base()}.
#' @param m_gam_full Fitted model from \code{fit_gam_full()}.
#' @return Data frame with columns: model, df, AIC, delta_AIC.
compare_aic <- function(m_pfr_int, m_pfr, m_gam_full) {

  aic_raw <- AIC(m_pfr_int, m_pfr, m_gam_full)

  result <- data.frame(
    model = c("m_pfr_int", "m_pfr", "m_gam_full"),
    df    = aic_raw$df,
    AIC   = aic_raw$AIC
  ) |>
    dplyr::arrange(AIC) |>
    dplyr::mutate(delta_AIC = AIC - min(AIC))

  message("AIC comparison:")
  print(result)
  result
}
