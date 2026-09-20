# =============================================================================
# f_model_plots.R — Analysis pipeline: visualisation
#
# All functions return a ggplot object.
# Retrieve any plot with:
#   targets::tar_read(p_beta0_curve, store = "_targets_analysis")
#
# Exported functions:
#   gg_term_variance(term_df)                    → bar chart
#   gg_swf_distance_effect(model, mod_data)      → SWF × distance contribution
#   gg_swf_main_effect(model, mod_data)          → baseline SWF contribution
#   gg_beta0_curve(model)                        → β₀(r) coefficient function
#   gg_beta_surface(model, distances)            → β(r,D) by tree-strip distance
# =============================================================================


# ── gg_term_variance ──────────────────────────────────────────────────────────

#' Horizontal bar chart of per-term variance proportions.
#'
#' @param term_df Data frame from \code{compute_term_variance()}.
#' @return ggplot object.
gg_term_variance <- function(term_df) {

  ggplot2::ggplot(
    term_df,
    ggplot2::aes(
      x = reorder(term, proportion),
      y = proportion
    )
  ) +
    ggplot2::geom_col(fill = "#4D7FA3") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x     = NULL,
      y     = "Var(term contribution) / Var(fitted values)",
      title = "Per-term variance proportions — m_pfr_int"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}


# ── gg_swf_distance_effect ────────────────────────────────────────────────────

#' Scatter plot of the SWF × distance interaction contribution (second
#' functional term in m_pfr_int) against tree-strip distance.
#'
#' @param model    Fitted m_pfr_int object.
#' @param mod_data Data frame from \code{add_pca_scores()}.
#' @return ggplot object.
gg_swf_distance_effect <- function(model, mod_data) {

  term_contrib <- stats::predict(model, type = "terms")

  plot_df <- data.frame(
    distance_to_tree_strip = mod_data$distance_to_tree_strip,
    swf_distance_effect    = term_contrib[, 2]   # column 2 = lf(swf_D, …)
  )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = distance_to_tree_strip, y = swf_distance_effect)
  ) +
    ggplot2::geom_point(alpha = 0.5, colour = "#4D7FA3") +
    ggplot2::geom_smooth(method = "lm", se = TRUE,
                         colour = "#1B4F72", fill = "#AED6F1") +
    ggplot2::labs(
      x     = "Distance to tree strip (m)",
      y     = "Fitted SWF × distance contribution",
      title = "SWF × tree-strip-distance interaction (functional term 2)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}


# ── gg_swf_main_effect ────────────────────────────────────────────────────────

#' Scatter plot of the baseline SWF functional contribution (first functional
#' term in m_pfr_int) against tree-strip distance.
#'
#' @param model    Fitted m_pfr_int object.
#' @param mod_data Data frame from \code{add_pca_scores()}.
#' @return ggplot object.
gg_swf_main_effect <- function(model, mod_data) {

  term_contrib <- stats::predict(model, type = "terms")

  plot_df <- data.frame(
    distance_to_tree_strip = mod_data$distance_to_tree_strip,
    swf_effect             = term_contrib[, 1]   # column 1 = lf(swf_mat, …)
  )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = distance_to_tree_strip, y = swf_effect)
  ) +
    ggplot2::geom_point(alpha = 0.5, colour = "#5D8A5E") +
    ggplot2::geom_smooth(method = "lm", se = TRUE,
                         colour = "#1E5631", fill = "#A9DFBF") +
    ggplot2::labs(
      x     = "Distance to tree strip (m)",
      y     = "Fitted SWF contribution",
      title = "Baseline SWF functional effect (functional term 1)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}


# ── gg_beta0_curve ────────────────────────────────────────────────────────────

#' Plot the estimated β₀(r) coefficient function (baseline SWF effect) with a
#' 95 % pointwise CI ribbon.
#'
#' @param model Fitted m_pfr_int object.
#' @return ggplot object.
gg_beta0_curve <- function(model) {

  beta0 <- stats::coef(model, select = 1)
  # coef.pfr returns a data frame with columns: <name>.argvals, value, se
  argvals_col <- grep("\\.argvals$", names(beta0), value = TRUE)

  plot_df <- data.frame(
    radius = beta0[[argvals_col]],
    value  = beta0$value,
    se     = beta0$se,
    ymin   = beta0$value - 1.96 * beta0$se,
    ymax   = beta0$value + 1.96 * beta0$se
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = radius)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      alpha = 0.20, fill = "#4D7FA3"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = value),
      linewidth = 1.0, colour = "#1B4F72"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = value),
      size = 2.5, colour = "#1B4F72"
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(100, 1000, by = 100),
      labels = scales::label_comma()
    ) +
    ggplot2::labs(
      x     = "SWF radius (m)",
      y     = expression(beta[0](r)),
      title = expression("Baseline SWF coefficient function " * beta[0](r)),
      subtitle = "Shaded band = 95 % pointwise CI"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}


# ── gg_beta_surface ───────────────────────────────────────────────────────────

#' Plot the combined coefficient function β(r, D) = β₀(r) + D·β₁(r) for a
#' set of representative tree-strip distances D.
#'
#' @param model     Fitted m_pfr_int object.
#' @param distances Numeric vector of tree-strip distances to show.
#'   Defaults to the five distances used in the original script.
#' @return ggplot object.
gg_beta_surface <- function(model,
                             distances = c(1, 4, 7, 12, 24)) {

  beta0 <- stats::coef(model, select = 1)
  beta1 <- stats::coef(model, select = 2)

  argvals_col <- grep("\\.argvals$", names(beta0), value = TRUE)
  radius_vals <- beta0[[argvals_col]]

  # Expand over all (radius, distance) combinations
  surface_df <- expand.grid(radius = radius_vals, distance = distances) |>
    dplyr::mutate(
      beta0 = rep(beta0$value, times = length(distances)),
      beta1 = rep(beta1$value, times = length(distances)),
      beta  = beta0 + distance * beta1
    )

  ggplot2::ggplot(
    surface_df,
    ggplot2::aes(
      x      = radius,
      y      = beta,
      group  = factor(distance),
      colour = factor(distance)
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_line(linewidth = 1.0) +
    ggplot2::scale_x_continuous(
      breaks = seq(100, 1000, by = 100),
      labels = scales::label_comma()
    ) +
    ggplot2::scale_colour_viridis_d(
      name   = "Distance to\ntree strip (m)",
      option = "D", end = 0.85
    ) +
    ggplot2::labs(
      x     = "SWF radius (m)",
      y     = expression(beta(r, D)),
      title = expression("SWF coefficient by radius and tree-strip distance"),
      subtitle = expression(beta(r, D) == beta[0](r) + D %*% beta[1](r))
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "right")
}
