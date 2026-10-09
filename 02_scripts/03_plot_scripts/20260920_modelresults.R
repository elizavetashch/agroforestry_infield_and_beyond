


m_swf2$coefficients

library(mgcv)
library(ggplot2)

# extract parametric terms (drop intercept)
fp <- as.data.frame(summary(m_3)$p.table) |>
  tibble::rownames_to_column("term") |>
  subset(term != "(Intercept)") |>
  transform(
    lwr = Estimate - 1.96 * `Std. Error`,
    upr = Estimate + 1.96 * `Std. Error`,
    sig = ifelse(`Pr(>|t|)` < 0.05, "p < 0.05", "p ≥ 0.05"),
    term = factor(term)  # sort by effect size
  )

# fp$term <- dplyr::recode(fp$term,
#                          swf_slope_0to200    = "SWF slope 0–200m",
#                          swf_slope_200to500  = "SWF slope 200–500m",
#                          swf_slope_500to1000 = "SWF slope 500–1000m",
#                          AFage               = "AF system age",
#                          PC1_c               = "PC1 climate",
#                          PC1_s               = "PC1 soil",
#                          l_shdi              = "Landscape diversity (SHDI)",
#                          l_ed                = "Edge density"
# )
# fp$term <- factor(fp$term, levels = fp$term[order(fp$Estimate)])

ggplot(fp, aes(x = Estimate, y = term, colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.25, linewidth = 0.8) +
  geom_point(size = 3.5) +
  scale_colour_manual(values = c("p < 0.05" = "#4ECDC4", "p ≥ 0.05" = "grey60")) +
  labs(
    title    = "Forest plot — parametric terms",
    subtitle = "Estimates ± 95% CI",
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

library(dplyr)
library(ggplot2)

distance_seq <- seq(
  min(mod_data$distance_to_tree_strip, na.rm = TRUE),
  max(mod_data$distance_to_tree_strip, na.rm = TRUE),
  length.out = 100
)

swf_seq <- seq(
  min(mod_data$swf_slope_500to1000, na.rm = TRUE),
  max(mod_data$swf_slope_500to1000, na.rm = TRUE),
  length.out = 100
)

newdat_3d <- expand.grid(
  distance_to_tree_strip = distance_seq,
  swf_slope_500to1000 = swf_seq
)

newdat_3d$swf_slope_0to200 <- mean(
  mod_data$swf_slope_0to200,
  na.rm = TRUE
)

newdat_3d$swf_slope_200to500 <- mean(
  mod_data$swf_slope_200to500,
  na.rm = TRUE
)

newdat_3d$treeage <- mean(
  mod_data$treeage,
  na.rm = TRUE
)

newdat_3d$AFage <- mean(
  mod_data$AFage,
  na.rm = TRUE
)

newdat_3d$PC1_c <- mean(
  mod_data$PC1_c,
  na.rm = TRUE
)

newdat_3d$PC1_s <- mean(
  mod_data$PC1_s,
  na.rm = TRUE
)

newdat_3d$l_shdi <- mean(
  mod_data$l_shdi,
  na.rm = TRUE
)

newdat_3d$l_ed <- mean(
  mod_data$l_ed,
  na.rm = TRUE
)

newdat_3d$year <- mod_data$year[1]

swf_mean <- colMeans(swf_mat, na.rm = TRUE)

newdat_3d$swf_mat <- matrix(
  rep(swf_mean, nrow(newdat_3d)),
  nrow = nrow(newdat_3d),
  byrow = TRUE
)



pred_3d <- predict(
  m_3,
  newdata = newdat_3d,
  type = "response"
)

newdat_3d$yield_pred <- as.numeric(pred_3d)



library(plotly)

z_matrix <- matrix(
  newdat_3d$yield_pred,
  nrow = length(distance_seq),
  ncol = length(swf_seq)
)

plot_ly(
  x = distance_seq,
  y = swf_seq,
  z = z_matrix,
  type = "surface"
) |>
  layout(
    scene = list(
      xaxis = list(
        title = "Distance from tree strip (m)"
      ),
      yaxis = list(
        title = "SWF slope 500–1000 m"
      ),
      zaxis = list(
        title = "Predicted relative yield"
      )
    )
  )
