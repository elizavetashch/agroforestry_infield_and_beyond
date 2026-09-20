# =============================================================================
# f_model_data.R — Analysis pipeline: data preparation
#
# Exported functions (called as individual targets):
#   prepare_dflong(df)          → dflong
#   prepare_swf_data(df)        → list(swf_unit, swf_summary, swf_wide)
#   join_mod_data(dflong, swf)  → mod_data (before PCA)
#   add_pca_scores(mod_data)    → mod_data + PC1_c + PC1_s
#   build_swf_inputs(mod_data)  → list(mat, swf_D, D, argvals)
# =============================================================================


# ── prepare_dflong ────────────────────────────────────────────────────────────

#' Aggregate the raw AF dataset to one row per field × year × crop × distance,
#' then derive modelling variables (photo_path, log_AFage, log_dist, yield_rel).
#'
#' @param df Raw data frame read from AF_swf.csv.
#' @return Aggregated and mutated data frame (dflong).
prepare_dflong <- function(df) {

  message("Analysis Step 1a — preparing dflong...")

  dflong <- df |>
    dplyr::group_by(field, year, crop_unified, distance_to_tree_strip) |>
    dplyr::summarise(
      yield_tha      = mean(yield_tha,      na.rm = TRUE),
      fert_n         = mean(fert_n,         na.rm = TRUE),
      temp_c_mean    = dplyr::first(temp_c_mean),
      precip_mm_sum  = dplyr::first(precip_mm_sum),
      sun_mj_m2_mean = dplyr::first(sun_mj_m2_mean),
      af_age         = dplyr::first(af_age),
      treeage        = dplyr::first(treeage),
      clay           = dplyr::first(clay),
      sand           = dplyr::first(sand),
      silt           = dplyr::first(silt),
      l_shdi         = dplyr::first(l_shdi),
      l_ed           = dplyr::first(l_ed),
      l_np           = dplyr::first(l_np),
      l_contag       = dplyr::first(l_contag),
      mean_slope     = dplyr::first(mean_slope),
      .groups        = "drop"
    ) |>
    dplyr::mutate(
      # C3 / C4 photosynthetic pathway (maize is the only C4 crop)
      photo_path   = factor(dplyr::if_else(crop_unified == "maize", "C4", "C3")),
      log_af_age   = log1p(af_age),
      log_dist     = log(distance_to_tree_strip),
      crop_unified = factor(crop_unified),
      field        = factor(field),
      year         = factor(year),
      dist_bin     = factor(distance_to_tree_strip),
      .by          = c(field, year, crop_unified)
    ) |>
    dplyr::group_by(field, year, crop_unified) |>
    dplyr::mutate(yield_rel = yield_tha / mean(yield_tha, na.rm = TRUE)) |>
    dplyr::ungroup()

  message(sprintf(
    "  dflong: %d rows | %d unique field×year×crop combinations",
    nrow(dflong),
    dplyr::n_distinct(dflong$field, dflong$year, dflong$crop_unified)
  ))
  message("  photo_path counts: ", paste(table(dflong$photo_path), collapse = " / "),
          " (C3 / C4)")

  dflong
}


# ── prepare_swf_data ──────────────────────────────────────────────────────────

#' Derive SWF radial profiles, field-level summaries, and a wide pivot table.
#'
#' @param df Raw data frame read from AF_swf.csv.
#' @return Named list with elements:
#'   \describe{
#'     \item{swf_unit}{Long table: field × year × distance × prop_swf.}
#'     \item{swf_summary}{One row per field × year: mean, max, nearfield prop, linear slope.}
#'     \item{swf_wide}{Wide table: one prop_swf column per 100 m step.}
#'   }
prepare_swf_data <- function(df) {

  message("Analysis Step 1b — preparing SWF radial profiles...")

  # Long table: one row per field × year × radius step
  swf_unit <- df |>
    dplyr::filter(!is.na(prop_swf)) |>
    dplyr::group_by(field, year, distance) |>
    dplyr::summarise(prop_swf = mean(prop_swf, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(field, year) |>
    dplyr::arrange(distance, .by_group = TRUE) |>
    dplyr::mutate(radius_rank = dplyr::row_number()) |>
    dplyr::ungroup()

  # Field-level SWF summaries
  swf_summary <- swf_unit |>
    dplyr::group_by(field, year) |>
    dplyr::summarise(
      swf_mean      = mean(prop_swf, na.rm = TRUE),
      swf_max       = max(prop_swf,  na.rm = TRUE),
      swf_nearfield = dplyr::first(prop_swf[distance == min(distance)]),
      swf_slope     = stats::coef(stats::lm(prop_swf ~ distance))[["distance"]],
      swf_n_radii   = dplyr::n(),
      .groups       = "drop"
    ) |>
    dplyr::mutate(year = as.factor(year))

  # Wide: one swf_d<distance> column per radius step (100, 200, …, 1000)
  swf_wide <- swf_unit |>
    tidyr::pivot_wider(
      id_cols     = c(field, year),
      names_from  = distance,
      names_prefix = "swf_d",
      values_from = prop_swf
    ) |>
    dplyr::mutate(year = as.factor(year))

  message("  swf_unit: ", nrow(swf_unit), " rows | swf_wide: ",
          ncol(swf_wide) - 2, " distance columns")

  list(swf_unit = swf_unit, swf_summary = swf_summary, swf_wide = swf_wide)
}


# ── join_mod_data ─────────────────────────────────────────────────────────────

#' Join the yield data frame with SWF summaries and the wide SWF matrix.
#'
#' @param dflong      Data frame from \code{prepare_dflong()}.
#' @param swf_prepared List from \code{prepare_swf_data()}.
#' @return Joined data frame (mod_data before PCA).
join_mod_data <- function(dflong, swf_prepared) {

  message("Analysis Step 1c — joining mod_data...")

  mod_data <- dflong |>
    dplyr::left_join(swf_prepared$swf_summary, by = c("field", "year")) |>
    dplyr::left_join(swf_prepared$swf_wide,    by = c("field", "year")) |>
    dplyr::mutate(
      field = as.factor(field),
      year  = as.factor(year)
    )

  message("  mod_data (pre-PCA): ", nrow(mod_data), " rows")
  mod_data
}


# ── add_pca_scores ────────────────────────────────────────────────────────────

#' Compute first principal components of the climate and soil-texture blocks
#' and join them to mod_data.
#'
#' Climate block : temp_c_mean, sun_mj_m2_mean, precip_mm_sum → PC1_c
#' Soil block    : clay, sand, silt                             → PC1_s
#'
#' @param mod_data Data frame from \code{join_mod_data()}.
#' @return mod_data extended with columns \code{PC1_c} and \code{PC1_s}.
add_pca_scores <- function(mod_data) {

  message("Analysis Step 1d — computing PCA scores...")

  # ── Climate PCA ───────────────────────────────────────────────────────────
  climate_mat <- scale(
    mod_data[, c("temp_c_mean", "sun_mj_m2_mean", "precip_mm_sum")]
  )
  pca_climate <- stats::princomp(climate_mat)

  climate_pca <- data.frame(
    field = mod_data$field,
    year  = mod_data$year,
    PC1_c = pca_climate$scores[, 1]
  ) |>
    dplyr::distinct()

  # ── Soil PCA ──────────────────────────────────────────────────────────────
  soil_mat <- scale(mod_data[, c("clay", "sand", "silt")])
  pca_soil <- stats::princomp(soil_mat)

  soil_pca <- data.frame(
    field = mod_data$field,
    year  = mod_data$year,
    PC1_s = pca_soil$scores[, 1]
  ) |>
    dplyr::distinct()

  # ── Join back ──────────────────────────────────────────────────────────────
  result <- mod_data |>
    dplyr::left_join(climate_pca, by = c("field", "year")) |>
    dplyr::left_join(soil_pca,   by = c("field", "year")) |>
    dplyr::mutate(
      field = as.factor(field),
      year  = as.factor(year)
    )

  message(sprintf(
    "  PC1_c explains %.1f%% | PC1_s explains %.1f%% of block variance",
    summary(pca_climate)$sdev[1]^2 / sum(summary(pca_climate)$sdev^2) * 100,
    summary(pca_soil)$sdev[1]^2   / sum(summary(pca_soil)$sdev^2)    * 100
  ))

  result
}


# ── build_swf_inputs ──────────────────────────────────────────────────────────

#' Assemble the functional predictor matrix and derived interaction matrix
#' needed by pfr().
#'
#' swf_mat  : n × 10 matrix of SWF proportions (columns = 100 m … 1 000 m).
#' D        : centred (mean-0) distance_to_tree_strip vector.
#' swf_D    : element-wise product swf_mat × D — used as the interaction
#'            functional term lf(swf_D, …) in m_pfr_int.
#' argvals  : radius argument vector 100, 200, …, 1 000.
#'
#' @param mod_data Data frame from \code{add_pca_scores()}.
#' @return Named list: mat, swf_D, D, argvals.
build_swf_inputs <- function(mod_data) {

  message("Analysis Step 1e — building SWF matrix...")

  swf_mat <- mod_data |>
    dplyr::select(dplyr::starts_with("swf_d")) |>
    as.matrix()

  argvals <- seq(from = 100, to = 1000, by = 100)
  stopifnot(ncol(swf_mat) == length(argvals))

  D     <- as.numeric(scale(mod_data$distance_to_tree_strip,
                             center = TRUE, scale = FALSE))
  swf_D <- swf_mat * D

  message(sprintf("  swf_mat: %d × %d | range [%.3f, %.3f]",
                  nrow(swf_mat), ncol(swf_mat),
                  min(swf_mat, na.rm = TRUE), max(swf_mat, na.rm = TRUE)))

  list(mat = swf_mat, swf_D = swf_D, D = D, argvals = argvals)
}
