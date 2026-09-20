


# Packages  ---------------------------------------------------------------


# Visualisation
library(ggplot2)
library(ggcorrplot)

# Data wrangling
library(dplyr)
library(tidyverse)
library(tibble)

# Multivariate / PCA
library(corrr)
library(FactoMineR)
library(factoextra)

# Modelling
library(mgcv)
library(refund)   # [F1] was loaded mid-script; moved here
library(gratia)   # [F1] was loaded mid-script; moved here



# Palette -----------------------------------------------------------------



fieldpalette <- c(
  "#577590", "#4d908e", "#43aa8b", "#90be6d",
  "#f9c74f", "#f8961e", "#f3722c", "#f94144"
)



# Datasets ----------------------------------------------------------------


df <- read.csv("01_Data/dffinal_20260916.csv")

df <- df |>
  mutate(
    yield_log     = log(yield_tha + 2),
    yield_scaled  = as.numeric(scale(yield_log)),
    distance_log  = log(distance_to_tree_strip),
    field         = as.factor(field),
    year          = as.factor(year),
    crop_unified  = as.factor(crop_unified),
    c3c4 = case_when(
      crop_unified == "maize" ~ as.factor("C3"),
      TRUE ~ as.factor("C4")
    )
  )

table(df$field)
colnames(df)

unique(df$crop_unified)

# Model -------------------------------------------------------------------

table(df$c3c4)

# H1 - distance to tree strip in relation to yield ------------------------

mod1 <- gamm(data = df, yield_log ~ s(distance_to_tree_strip, k = 3) * c3c4 + s(year, bs = "re"))

df <- df |> mutate(year = factor(year))

mod1 <- gamm(
  yield_log ~
    s(distance_to_tree_strip, by = c3c4, k = 3),
  random = list(field = ~1, year = ~1),
  data = df
)

summary(mod1$gam)

mod2 <- gamm(
  yield_log ~
    s(distance_to_tree_strip, by = crop_unified, k = 3),
  random = list(field = ~1, year = ~1),
  data = df
)

summary(mod2$gam)

# --- 1. model predictions on a fine grid -------------------------------------
pred_grid <- expand.grid(
  distance_to_tree_strip = seq(min(df$distance_to_tree_strip),
                               max(df$distance_to_tree_strip),
                               length.out = 200),
  c3c4  = levels(df$c3c4),
  field = levels(df$field)[1],  # arbitrary — random effect = 0 (population level)
  year  = df$year[1]
)

pred_grid$fit <- predict(mod1$gam, newdata = pred_grid, type = "response",
                         exclude = c("s(year)", "s(field)"))  # marginalise out RE

# get SE for ribbon
pred_se <- predict(mod1$gam, newdata = pred_grid, type = "response",
                   se.fit = TRUE,
                   exclude = c("s(year)", "s(field)"))
pred_grid$fit <- pred_se$fit
pred_grid$lwr <- pred_se$fit - 1.96 * pred_se$se.fit
pred_grid$upr <- pred_se$fit + 1.96 * pred_se$se.fit

# --- 2. per-field profiles (loess per field × c3c4) --------------------------
field_profiles <- df |>
  group_by(field, c3c4) |>
  arrange(distance_to_tree_strip) |>
  ungroup()

# --- 3. per-year profiles ----------------------------------------------------
year_profiles <- df |>
  mutate(year = factor(year)) |>
  group_by(year, c3c4) |>
  arrange(distance_to_tree_strip) |>
  ungroup()

# --- 4. plot ------------------------------------------------------------------
ggplot() +
  
  # raw data points
  geom_point(
    data  = df,
    aes(x = distance_to_tree_strip, y = yield_log, colour = c3c4),
    alpha = 0.15, size = 0.8, shape = 16
  ) +
  
  # per-field smooths
  geom_smooth(
    data    = field_profiles,
    aes(x = distance_to_tree_strip, y = yield_log,
        group = interaction(field, c3c4), colour = c3c4),
    method  = "loess", span = 0.9,
    se      = FALSE, linewidth = 0.4, alpha = 0.25
  ) +
  
  # per-year smooths
  geom_smooth(
    data    = year_profiles,
    aes(x = distance_to_tree_strip, y = yield_log,
        group = interaction(year, c3c4)),
    colour  = "grey40",
    method  = "loess", span = 0.9,
    se      = FALSE, linewidth = 0.35, alpha = 0.2, linetype = "dashed"
  ) +
  
  # model CI ribbon
  geom_ribbon(
    data = pred_grid,
    aes(x = distance_to_tree_strip, ymin = lwr, ymax = upr, fill = c3c4),
    alpha = 0.2
  ) +
  
  # model mean line
  geom_line(
    data = pred_grid,
    aes(x = distance_to_tree_strip, y = fit, colour = c3c4),
    linewidth = 1.2
  ) +
  
  scale_colour_manual(values = c("C3" = "#2196F3", "C4" = "#E91E63"),
                      name = "Pathway") +
  scale_fill_manual(values   = c("C3" = "#2196F3", "C4" = "#E91E63"),
                    name = "Pathway") +
  labs(
    x     = "Distance from tree strip (m)",
    y     = "Yield (log)",
    title = "GAMM: Yield ~ s(distance) × C3/C4",
    caption = "Dashed grey = year profiles · Coloured thin = field profiles"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")













# I believe that field characteristica influence the yield. 
# l_shdi: the shannons index will have a positive effect on the yield
# PC1_s: the more clay in the soil the better
# PC1_c: the warmer and wetter the better
# mean_slope: the bigger the value the bigger yield
# crop unified is the Group factor. Relationships will differ across them, 
# and for every crop there is its own relationship
# so it is (distance|crop)
# swf have a positive effect on yield. Negative when they are closest, 
# and positive when it's far away. 

# i dont know what to do with small woody features, because there are three values
# per field. i cannot use a predictor that has three unique values. 
# but it can be fixed. so the function will be built, but included as a fixed effect. 
# it is the same for some other fields. 
# it is more the characteristica of the field. 


m_int2 <- pfr(
  yield_log ~
    lf(swf_matrix,      argvals = swf_argvals, k = 8) +
    lf(swf_x_field,      argvals = swf_argvals, k = 8) +
    lf(swf_x_field_distyield, argvals = swf_argvals, k = 8) +
    s(log(distance_to_tree_strip), k = 3) +
    s(prop_swf_within) +
    s(l_shdi) +
    PC1_s + PC1_c +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") +
    s(year,  bs = "re"),
  data   = z_swf,
  method = "REML"
)
summary(m_int2)
AIC(m3_swf_functional, m_int1, m_int2)













library(tidyverse)
library(lme4)
library(lmerTest) 

annulus_long <- annulus_all |>
  pivot_longer(
    cols      = matches("^(prop_swf|n_swf)\\."),
    names_to  = c(".value", "year"),
    names_sep = "\\.") |>
  mutate(year = as.integer(year)) |> 
  mutate(year = as.factor(year))
  


swf_zones <- annulus_long |>
  mutate(zone = case_when(
    distance < 300          ~ "near",
    distance >= 300 & distance < 700 ~ "mid",
    distance >= 700         ~ "far"
  )) |>
  group_by(id, year, zone) |>
  summarise(n_swf = sum(n_swf, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from  = zone,
    values_from = n_swf,
    names_prefix = "n_swf_"
  )

df_model <- df |>
  left_join(swf_zones, by = c("field" = "id", "year")) |>
  mutate(
    # scale continuous predictors — important for interaction interpretability
    dist_s       = scale(distance_to_tree_strip),
    n_swf_near_s = scale(n_swf_near),
    n_swf_far_s  = scale(n_swf_far)
  )

mod2 <- lmer(
  yield_log ~
    dist_s * n_swf_near_s +   # H1a: high near-SWF suppresses yield at large distance
    dist_s * n_swf_far_s  +   # H1b: high far-SWF boosts max yield
    (dist_s|crop_unified) +             # fixed: crop has large main effect on yield level
    (1 | field) +              # random: between-field baseline differences
    (1 | year),                # random: between-year climate/management variation
  data = df_model
)

summary(mod2)
library(ggeffects)

# H1a: does near-SWF × distance interaction hold?
plot(ggpredict(mod2, terms = c("dist_s [all]", "n_swf_near_s [-1, 0, 1]")))

# H1b: far-SWF × distance
plot(ggpredict(mod2, terms = c("dist_s [all]", "n_swf_far_s [-1, 0, 1]")))



# pfr  --------------------------------------------------------------------

library(tidyverse)
library(refund)

# common distance grid all fields will be interpolated onto
d_grid <- seq(0, 1000, by = 25)   # adjust to your range

swf_mat <- annulus_long |>
  group_by(id, distance) |>
  summarise(n_swf_mean = mean(n_swf, na.rm = TRUE), .groups = "drop") |>
  group_by(id) |>
  # interpolate each field onto the common grid
  reframe(
    d     = d_grid,
    n_swf = approx(distance, n_swf_mean, xout = d_grid, rule = 2)$y
  ) |>
  pivot_wider(names_from = d, values_from = n_swf) |>
  column_to_rownames("id")

# result: matrix [n_fields × n_grid_points]
swf_mat <- as.matrix(swf_mat)


# this doesnt make any sense with the mean yield 
# try to do the same plot of the functional effect of the distance with 
# indiwidual swf field profiles AND
# also the plot of the effect of swf on the distance"s effect on yield
# but manage the data correctly, because now it makes no sense. 


yield_scalar <- df |>
  group_by(field, year, crop_unified) |>
  summarise(yield_mean = mean(yield_log, na.rm = TRUE)) |>
  arrange(match(field, rownames(swf_mat)))  # align row order!

model_pfr <- pfr(
  yield_mean ~ lf(swf_mat, argvals = d_grid) +   # lf = linear functional term
  crop_unified,
  data = as.data.frame(yield_scalar)
)

summary(model_pfr)

plot(model_pfr, shade = TRUE,
     xlab = "Distance from tree row (m)",
     ylab = expression(beta(distance)),
     main = "Effect of SWF profile on mean yield")
abline(h = 0, lty = 2, col = "grey50")

library(ggrepel)

# label at the rightmost distance point per field
swf_labels <- swf_long |> 
  group_by(field) |> 
  slice_max(distance, n = 1)

# add to plot:
geom_text_repel(
  data = swf_labels,
  aes(x = distance, y = n_swf_scaled, label = field, colour = field),
  size = 3, nudge_x = 20, show.legend = FALSE
)


# effect of swf profile on mean yield over distance -----------------------

library(tidyverse)

# --- 1. extract β(d) from the model -------------------------------------------
# --- 1. extract β(d) from the model -------------------------------------------
beta_raw <- coef(model_pfr)

beta_df <- data.frame(
  distance = beta_raw$swf_mat.argvals,
  beta     = beta_raw$value,
  se       = beta_raw$se
)

# --- 2. rescale raw SWF curves to fit β axis ----------------------------------
swf_long <- as.data.frame(swf_mat) |>
  rownames_to_column("field") |>
  pivot_longer(-field, names_to = "distance", values_to = "n_swf") |>
  mutate(distance = as.numeric(distance))

# linear rescale: map SWF range → β range
beta_range <- range(beta_df$beta, na.rm = TRUE)
swf_range  <- range(swf_long$n_swf, na.rm = TRUE)

rescale_swf <- function(x) {
  (x - swf_range[1]) / diff(swf_range) * diff(beta_range) + beta_range[1]
}

swf_long <- swf_long |>
  mutate(n_swf_scaled = rescale_swf(n_swf))

# --- 3. plot ------------------------------------------------------------------
ggplot() +
  # individual field SWF curves (rescaled, in background)
  geom_line(
    data = swf_long,
    aes(x = distance, y = n_swf_scaled, group = field, colour = field),
    linewidth = 0.5, alpha = 0.5
  ) +
  # β confidence ribbon
  geom_ribbon(
    data = beta_df,
    aes(x = distance, ymin = beta - 1.96 * se, ymax = beta + 1.96 * se),
    fill = "grey30", alpha = 0.2
  ) +
  # β curve
  geom_line(
    data = beta_df,
    aes(x = distance, y = beta),
    colour = "black", linewidth = 1.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  # secondary axis label (purely cosmetic — shows SWF scale)
  scale_y_continuous(
    name     = expression(beta(distance)),
    sec.axis = sec_axis(
      transform = ~ (. - beta_range[1]) / diff(beta_range) * diff(swf_range) + swf_range[1],
      name  = "N_SWF (mean across years)"
    )
  ) +
  scale_colour_brewer(palette = "Set2", name = "Field") +
  labs(
    x     = "Distance from tree row (m)",
    title = expression("Functional effect  " * beta(d) * "  with individual field SWF profiles")
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position   = "bottom",
    axis.title.y.right = element_text(colour = "grey50"),
    axis.text.y.right  = element_text(colour = "grey50")
  )
