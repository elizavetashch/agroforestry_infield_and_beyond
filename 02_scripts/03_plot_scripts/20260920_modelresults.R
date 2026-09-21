


m_swf2$coefficients

library(mgcv)
library(ggplot2)

# extract parametric terms (drop intercept)
fp <- as.data.frame(summary(m_swf3)$p.table) |>
  tibble::rownames_to_column("term") |>
  subset(term != "(Intercept)") |>
  transform(
    lwr = Estimate - 1.96 * `Std. Error`,
    upr = Estimate + 1.96 * `Std. Error`,
    sig = ifelse(`Pr(>|t|)` < 0.05, "p < 0.05", "p ≥ 0.05"),
    term = factor(term)  # sort by effect size
  )

fp$term <- dplyr::recode(fp$term,
                         swf_slope_0to200    = "SWF slope 0–200m",
                         swf_slope_200to500  = "SWF slope 200–500m",
                         swf_slope_500to1000 = "SWF slope 500–1000m",
                         AFage               = "AF system age",
                         PC1_c               = "PC1 climate",
                         PC1_s               = "PC1 soil",
                         l_shdi              = "Landscape diversity (SHDI)",
                         l_ed                = "Edge density"
)
fp$term <- factor(fp$term, levels = fp$term[order(fp$Estimate)])

ggplot(fp, aes(x = Estimate, y = term, colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.25, linewidth = 0.8) +
  geom_point(size = 3.5) +
  scale_colour_manual(values = c("p < 0.05" = "#4ECDC4", "p ≥ 0.05" = "grey60")) +
  labs(
    title    = "Forest plot — parametric terms",
    subtitle = "Estimates ± 95% CI from GAM m_swf2",
    x        = "Effect on relative yield",
    y        = NULL,
    colour   = NULL
  ) +
  #coord_cartesian(xlim = c(-1, 1)) +
  theme_bw(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())
  

# extract parametric terms (drop intercept)
fp <- as.data.frame(summary(m_pfr)$p.table) |>
  tibble::rownames_to_column("term") |>
  subset(term != "(Intercept)") |>
  transform(
    lwr = Estimate - 1.96 * `Std. Error`,
    upr = Estimate + 1.96 * `Std. Error`,
    sig = ifelse(`Pr(>|t|)` < 0.05, "p < 0.05", "p ≥ 0.05"),
    term = factor(term, levels = term[order(Estimate)])  # sort by effect size
  )

ggplot(fp, aes(x = Estimate, y = term, colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.25, linewidth = 0.8) +
  geom_point(size = 3.5) +
  scale_colour_manual(values = c("p < 0.05" = "#4ECDC4", "p ≥ 0.05" = "grey60")) +
  labs(
    title    = "Forest plot — parametric terms",
    subtitle = "Estimates ± 95% CI from GAM m_swf2",
    x        = "Effect on relative yield",
    y        = NULL,
    colour   = NULL
  ) +
  #coord_cartesian(xlim = c(-1, 1)) +
  theme_bw(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())






# pfr model results  ------------------------------------------------------

library(gratia)

smooth_estimates(m_pfr, smooth = "lf(swf_mat, argvals = swf_argvals, k = 5)", partial_match = TRUE) |>
  ggplot(aes(x = swf_mat, y = .estimate)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(x = "SWF radius (argvals)", y = "β(r) — effect on yield (t/ha)") +
  theme_bw()
  

