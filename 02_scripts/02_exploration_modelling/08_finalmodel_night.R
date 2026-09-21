

# preparation -------------------------------------------------------------


library(refund)   
library(mgcv)    
library(tidyverse)
library(gratia) 

mod_data <- read.csv("01_Data/20260920_moddata.csv")
swf_mat <- read.csv("01_Data/20260920_swf_mat.csv")

mod_data$year <- as.factor(mod_data$year)
mod_data$field <- as.factor(mod_data$field)
mod_data$photo_path <- as.factor(mod_data$photo_path)

radii <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)

swf_mat <- as.matrix(mod_data[, paste0("swf_d", radii)])

mod_data$swf_slope_0to200 <- apply(
  swf_mat[, radii >= 100 & radii <= 200, drop = FALSE],
  1,
  function(y) coef(lm(y ~ radii[radii >= 100 & radii <= 200]))[2]
)

mod_data$swf_slope_200to500 <- apply(
  swf_mat[, radii >= 200 & radii <= 500, drop = FALSE],
  1,
  function(y) coef(lm(y ~ radii[radii >= 200 & radii <= 500]))[2]
)

mod_data$swf_slope_500to1000 <- apply(
  swf_mat[, radii >= 500 & radii <= 1000, drop = FALSE],
  1,
  function(y) coef(lm(y ~ radii[radii >= 500 & radii <= 1000]))[2]
)


summary(mod_data)


# easiest model -----------------------------------------------------------

m_0 <- gam(yield_rel ~ s(distance_to_tree_strip, k = 5) + s(year, bs = "re"),
    data = mod_data,
    family = gaussian,
    method = "REML"
)

summary(m_0)
k.check(m_0)
par(mfrow = c(2, 2))
gam.check(m_0, pch = 16, cex = 0.5)
par(mfrow = c(1, 1))
plot(m_0, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE, scale = 0, residuals = TRUE,
     pch = 16, cex = 0.3, col = "grey50")

AIC(m_0)
# interaction model -----------------------------------------------------------

m_1 <- gam(yield_rel ~ s(distance_to_tree_strip, k = 5) + 
             swf_slope_0to200 +
             swf_slope_200to500 + 
             swf_slope_500to1000 + 
             ti(distance_to_tree_strip, swf_slope_0to200, k = c(5, 5)) +
             s(year, bs = "re"),
    data = mod_data,
    family = gaussian,
    method = "REML"
)

summary(m_1)
k.check(m_1)
par(mfrow = c(2, 2))
gam.check(m_1, pch = 16, cex = 0.5)
par(mfrow = c(1, 1))
plot(m_1, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE, scale = 0, residuals = TRUE,
     pch = 16, cex = 0.3, col = "grey50")

AIC(m_1)
# interaction model with field characteristics -----------------------------------------------------------

m_2 <- gam(yield_rel ~ s(distance_to_tree_strip, k = 5) + 
             swf_slope_0to200 +
             swf_slope_200to500 + 
             swf_slope_500to1000 + 
             ti(distance_to_tree_strip, swf_slope_0to200, k = c(5, 5)) +
             ti(distance_to_tree_strip, treeage, k = c(3, 3)) +
             treeage + 
             AFage +
             PC1_c +
             PC1_s +
             l_shdi +
             l_ed +
             s(field, bs = "re") +
             s(year, bs = "re"),
    data = mod_data,
    family = gaussian,
    method = "REML"
)

summary(m_2)
k.check(m_2)
par(mfrow = c(2, 2))
gam.check(m_2, pch = 16, cex = 0.5)
par(mfrow = c(1, 1))
plot(m_2, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE, scale = 0, residuals = TRUE,
     pch = 16, cex = 0.3, col = "grey50")

AIC(m_2)



# plots -------------------------------------------------------------------

library(ggplot2)

## 1. Parametric coefficients forest plot

coefs <- summary(m_2)$p.table

coef_df <- data.frame(
  term = rownames(coefs),
  estimate = coefs[, "Estimate"],
  se = coefs[, "Std. Error"]
)

coef_df <- coef_df[coef_df$term != "(Intercept)", ]

coef_df$lower <- coef_df$estimate - 1.96 * coef_df$se
coef_df$upper <- coef_df$estimate + 1.96 * coef_df$se

ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2
  ) +
  geom_point(size = 3) +
  labs(
    x = "Model coefficient (95% CI)",
    y = NULL
  ) +
  theme_classic()
