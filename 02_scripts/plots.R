
df <- read.csv("01_Data/moddata.csv")
swf_long <- read.csv("01_Data/20260920_swf_mat.csv")
library(dplyr); library(tidyr); library(ggplot2); library(patchwork)

vars <- c("l_shdi", "l_ed", "l_contag", "treeage", "AFage", "PC1_c", "PC1_s")

field_vars <- df %>%
  group_by(field) %>%
  summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-field, names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  mutate(z = (value - mean(value)) / sd(value)) %>%
  ungroup()

df %>%
  group_by(field) %>%
  summarise(across(all_of(vars), n_distinct))

df %>%
  group_by(field, distance_to_tree_strip) %>%
  summarise(across(yield_rel, n_distinct)) |> 
  print(n = 100)

p_yield <- ggplot(moddata, aes(distance_to_tree_strip, yield_rel)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "loess", colour = "darkgreen") +
  facet_wrap(~ field, nrow = 2) +
  labs(x = "Distance to tree strip (m)", y = "Relative yield") +
  theme_minimal()




cf <- coef(m_global)
lin_vars <- c("l_shdi", "l_ed", "l_contag", "treeage", "AFage", "PC1_c", "PC1_s")

# linear terms: coefficient x (field mean - overall mean)
lin_contrib <- df %>%
  group_by(field) %>%
  summarise(across(all_of(lin_vars), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(across(all_of(lin_vars), ~ (.x - mean(.x)) * cf[cur_column()])) %>%
  pivot_longer(-field, names_to = "term", values_to = "contribution")

# functional SWF term: field mean contribution minus overall mean
swf_term <- predict(m_global, type = "terms")[, "s(R):SWF"]
swf_contrib <- tibble(field = dflong$field, c = swf_term) %>%
  group_by(field) %>% summarise(c = mean(c)) %>%
  mutate(term = "SWF (all radii)", contribution = c - mean(c)) %>%
  select(field, term, contribution)

contrib <- bind_rows(lin_contrib, swf_contrib)

p_contrib <- ggplot(contrib, aes(contribution, term, fill = contribution > 0)) +
  geom_col() +
  geom_vline(xintercept = 0) +
  facet_wrap(~ field, nrow = 2) +
  scale_fill_manual(values = c("#2166ac", "#b2182b"), guide = "none") +
  labs(x = "Effect on predicted relative yield vs. average field", y = NULL) +
  theme_minimal()




swf_long <- df %>%
  select(field, swf_year, starts_with("SWF."), starts_with("R.")) %>%
  distinct() %>%                                   # drop the per-plot duplicates
  rename_with(~ paste0("SWF_", seq_along(.x)), starts_with("SWF.")) %>%   # SWF_1 ... SWF_10
  rename_with(~ sub("R.", "R_", .x, fixed = TRUE), starts_with("R.")) %>% # R_1 ... R_10
  pivot_longer(
    cols      = -c(field, swf_year),
    names_to  = c(".value", "idx"),
    names_sep = "_"
  ) %>%
  rename(radius = R) %>%
  select(field, swf_year, radius, SWF)


# Field Page  --------------------------------------------------------------------

field_page <- function(f) {
  a <- ggplot(swf_long, aes(radius, SWF)) +
    geom_smooth(aes(group = field), colour = "grey80",
                se = FALSE, method = "loess", formula = y ~ x) +
    geom_smooth(data = filter(swf_long, field == f),
                aes(group = swf_year), colour = "darkgreen", linetype = "dotted",
                linewidth = 0.7, se = FALSE, method = "loess", formula = y ~ x) +
    geom_smooth(data = filter(swf_long, field == f),
                colour = "darkgreen", linewidth = 1.2,
                se = FALSE, method = "loess", formula = y ~ x) +
    labs(title = "SWF profile", x = "Radius (m)") + theme_minimal()
  
  b <- ggplot(filter(df, field == f), aes(distance_to_tree_strip, yield_rel)) +
    geom_point(alpha = 0.3) +
    stat_summary(fun = mean, geom = "line", colour = "darkgreen", linewidth = 1) +
    stat_summary(fun.data = mean_se, geom = "pointrange", colour = "darkgreen") +
    labs(title = "Yield vs. distance", x = "Distance (m)") + theme_minimal() 
  
  c <- ggplot(filter(contrib, field == f),
              aes(contribution, reorder(term, contribution), fill = contribution > 0)) +
    geom_col() + geom_vline(xintercept = 0) +
    scale_fill_manual(values = c("#2166ac", "#b2182b"), guide = "none") +
    labs(title = "Model contributions", x = "Δ predicted yield", y = NULL) + theme_minimal()
  
  d <- ggplot(field_vars, aes(z, variable)) +
    geom_point(colour = "grey75") +
    geom_point(data = filter(field_vars, field == f), colour = "darkgreen", size = 3) +
    geom_vline(xintercept = 0, linetype = 2) +
    labs(title = "Covariates (z vs. other fields)", x = "z-score", y = NULL) + theme_minimal()
  
  (a | b) / (d | c) + plot_annotation(title = paste("Field", f))
}

pdf("field_portfolios.pdf", width = 11, height = 8)
for (f in sort(unique(df$field))) print(field_page(f))
dev.off()




# 3D graph ----------------------------------------------------------------
# this graph doesnt make sense 

library(plotly)

radii <- seq(100, 1000, by = 100)
moddata$SWF_mean <- rowMeans(moddata$SWF)
# alternatives:
# moddata$SWF_300 <- moddata$SWF[, radii == 300]
# moddata$SWF_eff <- predict(m_global, type = "terms")[, "s(R):SWF"]

plot_ly(moddata,
        x = ~distance_to_tree_strip, y = ~SWF_mean, z = ~yield_rel,
        color = ~field, type = "scatter3d", mode = "markers",
        marker = list(size = 3)) %>%
  layout(scene = list(xaxis = list(title = "Distance (m)"),
                      yaxis = list(title = "Mean SWF"),
                      zaxis = list(title = "Relative yield")))

