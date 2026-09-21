

library(terra)
# plots for the presentation 

rfile <- "C:/Users/Elizaveta/OneDrive - Universität Bayreuth/Dokumente/MasterThesis/MA_RProject/01_Data/AnalysisData/LULC/Wendhausen_field_2021_buf3000m.tif"
r <- terra::rast(rfile)
plot(r)


m_swf <- gam(
  yield_rel ~
    s(distance_to_tree_strip, by =  photo_path, k = 5) +
    swf_slope_0to200 +
    swf_slope_200to500 +
    swf_slope_500to1000 +
    ti(distance_to_tree_strip, swf_slope_0to200, k = c(5, 5)) +
    ti(distance_to_tree_strip, treeage, k = c(3, 3)) +
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

summary(m_swf)

# 6.1 Basis Dimension Adequacy
# Rule: k-index < 1 AND p < 0.05 → basis too restrictive; increase k in s().
k.check(m_swf)

# 6.2 Residual Diagnostics
# Plot 	Good Sign
# QQ Plot 	Points on the diagonal
# Residuals vs Fitted 	Random scatter around zero
# Histogram 	Bell-shaped, symmetric
# Response vs Fitted 	Points on the 1:1 line

par(mfrow = c(2, 2))
gam.check(m_swf, pch = 16, cex = 0.5)
par(mfrow = c(1, 1))

# 6.3 Concurvity
# Rule: Values near 1 → one smooth approximated by others → unstable estimates. Values < 0.8 are acceptable.
concurvity(m_swf, full = TRUE)

plot(m_swf, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE, scale = 0, residuals = TRUE,
     pch = 16, cex = 0.3, col = "grey50")
AIC(m_swf)

# marginal curve
p  <- predict(m_swf2, newdata=mod_data, type="response", se.fit=TRUE)
mod_data$fit   <- p$fit
mod_data$lower <- p$fit - 1.96*p$se.fit
mod_data$upper <- p$fit + 1.96*p$se.fit

ggplot(mod_data, aes(distance_to_tree_strip, fit)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2, fill="steelblue") +
  geom_line(color="steelblue", linewidth=1.2) +
  geom_rug(data=df, aes(x=distance_to_tree_strip), inherit.aes=FALSE, alpha=0.15) +
  labs(title="Marginal Effect: Temperature on Species Abundance",
       subtitle="Rainfall & Elevation at medians | 95% CI shaded",
       x="Temperature (°C)", y="Predicted Abundance") +
  theme_minimal(base_size=12)

# summaries
s <- summary(m_swf2)
print(round(s$s.table, 3))

# model swf without field random effect  ----------------------------------

m_swf2 <- gam(
  yield_rel ~
    s(distance_to_tree_strip, by =  crop_unified, k = 5) +
    swf_slope_0to200 +
    swf_slope_200to500 +
    swf_slope_500to1000 +
    ti(distance_to_tree_strip, swf_slope_0to200, k = c(5, 5)) +
    ti(distance_to_tree_strip, treeage, k = c(3, 3)) +
    AFage +
    PC1_c +
    PC1_s +
    l_shdi +
    l_ed +
    s(year, bs = "re"),
  data = mod_data,
  family = gaussian,
  method = "REML"
)

AIC(m_swf2)
s <- summary(m_swf2)
k.check(m_swf2)

par(mfrow = c(2, 2))
gam.check(m_swf2, pch = 16, cex = 0.5)
par(mfrow = c(1, 1))

as.table(concurvity(m_swf2, full = TRUE))


# mod 4  ------------------------------------------------------------------


m_swf4 <- gam(
  yield_rel ~
    s(distance_to_tree_strip, by =  photo_path, k = 5) +
    
    prop_swf_within +
    swf_slope_0to200:prop_swf_within + 
    
    swf_slope_200to500 +
    swf_slope_500to1000 +
    ti(distance_to_tree_strip, swf_slope_0to200, k = c(5, 5)) +
    ti(distance_to_tree_strip, treeage, k = c(3, 3)) +
    AFage +
    PC1_c +
    PC1_s +
    l_shdi +
    l_ed +
    s(year, bs = "re"),
  data = mod_data,
  family = gaussian,
  method = "REML"
)

AIC(m_swf4)
summary(m_swf4)
k.check(m_swf4)

par(mfrow = c(2, 2))
gam.check(m_swf4, pch = 16, cex = 0.5)
par(mfrow = c(1, 1))

as.table(concurvity(m_swf4, full = TRUE))



# model3 with swf einzeln -------------------------------------------------

m_swf3 <- gam(
  yield_rel ~
    s(distance_to_tree_strip, by =  photo_path, k = 5) +
    swf_d100 +
    swf_d200 +
    swf_d300 +
    swf_d400 +
    swf_d500 +
    swf_d600 +
    swf_d700 +
    swf_d800 +
    swf_d900 +
    swf_d1000 +
    ti(distance_to_tree_strip, treeage, k = c(3, 3)) +
    AFage +
    PC1_c +
    PC1_s +
    l_shdi +
    l_ed +
    s(year, bs = "re"),
  data = mod_data,
  family = gaussian,
  method = "REML"
)

AIC(m_swf3)

k.check(m_swf3)

par(mfrow = c(2, 2))
gam.check(m_swf3, pch = 16, cex = 0.5)
par(mfrow = c(1, 1))

as.table(concurvity(m_swf3, full = TRUE))

summary(m_swf3)



# distance to relative yield  ---------------------------------------------

ggplot(
  mod_data,
  aes(
    x = distance_to_tree_strip,
    y = yield_rel
  )
) +
  geom_point(alpha = 0.4) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = TRUE
  ) +
  labs(
    x = "Distance to tree strip (m)",
    y = "Relative yield",
    colour = "Photo path"
  ) +
  theme_minimal(base_size = 14)





# model pfr ---------------------------------------------------------------


m_pfr <- pfr(
  yield_tha ~
    lf(swf_mat, argvals = swf_argvals, k = 5) +
    s(distance_to_tree_strip, by = photo_path, k = 5) +
    ti(distance_to_tree_strip, treeage, k = c(5, 5)) +
    AFage +
    PC1_c +
    PC1_s +
    l_shdi +
    l_ed +
    s(year, bs = "re"),
  data = mod_data,
  method = "REML"
)

par(mfrow = c(2, 2))
gam.check(m_pfr)
summary(m_pfr)

AIC(m_pfr_int, m_pfr)



# mod5 final --------------------------------------------------------------

m_swf4_revised <- gam(
  yield_rel ~
    s(distance_to_tree_strip, photo_path, bs = "fs", k = 5) +
    swf_slope_0to200 + 
    swf_slope_200to500 +
    swf_slope_500to1000 +
    ti(distance_to_tree_strip, swf_slope_0to200, k = c(5, 5)) +
    prop_swf_within +
    swf_slope_0to200:prop_swf_within +   
    ti(distance_to_tree_strip, treeage, k = c(5, 4)) +
    treeage + AFage + 
    PC1_c + PC1_s +
    l_shdi +
    l_ed +
    s(year, bs = "re"),
  data = mod_data,
  family = gaussian,
  method = "REML"
)

AIC(m_swf4_revised)
  

AIC(m_swf2)
s <- summary(m_swf4_revised)
k.check(m_swf4_revised)

par(mfrow = c(2, 2))
gam.check(m_swf4_revised, pch = 16, cex = 0.5)
par(mfrow = c(1, 1))

as.table(concurvity(m_swf4_revised, full = TRUE))
