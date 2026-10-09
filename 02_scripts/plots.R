
df <- read.csv("01_Data/20261009_moddata")

library(dplyr); library(tidyr); library(ggplot2); library(patchwork)

vars <- c("l_shdi", "l_ed", "l_contag", "treeage", "AFage", "PC1_c", "PC1_s")

df %>%
  group_by(field) %>%
  summarise(across(all_of(vars), n_distinct))

p_yield <- ggplot(dflong, aes(distance_to_tree_strip, yield_rel)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "loess", colour = "darkgreen") +
  facet_wrap(~ field, nrow = 2) +
  labs(x = "Distance to tree strip (m)", y = "Relative yield") +
  theme_minimal()




cf <- coef(m_global)
lin_vars <- c("l_shdi", "l_ed", "l_contag", "treeage", "AFage", "PC1_c", "PC1_s")

# linear terms: coefficient x (field mean - overall mean)
lin_contrib <- dflong %>%
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




# Field Page  --------------------------------------------------------------------

field_page <- function(f) {
  a <- ggplot(swf_long, aes(radius, SWF, group = field)) +
    geom_line(colour = "grey80") +
    geom_line(data = filter(swf_long, field == f), colour = "darkgreen", linewidth = 1.2) +
    labs(title = "SWF profile", x = "Radius (m)") + theme_minimal()
  
  b <- ggplot(filter(dflong, field == f), aes(distance_to_tree_strip, yield_rel)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "gam", formula = y ~ s(x, k = 5), colour = "darkgreen") +
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
for (f in sort(unique(dflong$field))) print(field_page(f))
dev.off()
