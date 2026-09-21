

library(refund)   
library(mgcv)    
library(tidyverse)
library(gratia)    
library(broom) 

mod_data <- read.csv("01_Data/20260920_moddata.csv")
mod_data <- mod_data |> mutate(distance_to_tree_strip_changed = 
  case_when(
    distance_to_tree_strip == 4.5 ~ 4,
    distance_to_tree_strip == 9 ~ 7,
    TRUE ~ distance_to_tree_strip 
  )
) |> 
  mutate(year = as.factor(year),
         field = as.factor(field),
         crop_unified = as.factor(crop_unified)) |> 
  mutate(photo_path   = factor(ifelse(crop_unified == "maize", "C4", "C3")))

swf_mat <- read.csv("01_Data/20260920_swf_mat.csv")


# calculate slopes for swf ------------------------------------------------

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
summary(mod_data$swf_slope_0to200)
summary(mod_data$swf_slope_200to500)
summary(mod_data$swf_slope_500to1000)

# model only distance -----------------------------------------------------


m_dist <- gam(
  yield_rel ~
  s(distance_to_tree_strip, k = 5) +
  s(field, bs = "re") +
  s(year, bs = "re"),
  data   = mod_data, 
  family=gaussian(),
  method = "REML"
)

par(mfrow = c(2,2))
summary(m_dist)
gam.check(m_dist)


par(mfrow=c(1,2))
qq.gam(m_dist, main="normal", rep=200, asp=1)
k.check(m_dist)
# k-index < 1 AND p < 0.05 → basis too restrictive; increase k in s()

plot(m_dist, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE, scale = 0, residuals = TRUE,
     pch = 16, cex = 0.3, col = "grey50")


m_noswf <- gam(
  yield_rel ~
    s(distance_to_tree_strip, k = 5) +
    ti(distance_to_tree_strip, treeage, k = c(3, 3)) +
    AFage +
    PC1_c +
    PC1_s +
    l_shdi +
    l_ed +
    s(field, bs = "re") +
    s(year, bs = "re"),
  data   = mod_data, 
  family=gaussian(),
  method = "REML"
)


par(mfrow = c(2,2))
summary(m_noswf)
gam.check(m_noswf)


par(mfrow=c(2,2))
qq.gam(m_noswf, main="normal", rep=200, asp=1)
k.check(m_noswf)
# k-index < 1 AND p < 0.05 → basis too restrictive; increase k in s()

plot(m_noswf, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE, scale = 0, residuals = TRUE,
     pch = 16, cex = 0.3, col = "grey50")

AIC(m_dist, m_noswf)

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
  family = gaussian(),
  method = "REML"
)

summary(m_swf)
gam.check(m_swf)
k.check(m_swf)
plot(m_swf, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE, scale = 0, residuals = TRUE,
     pch = 16, cex = 0.3, col = "grey50")
AIC(m_swf)



# glmm comparision --------------------------------------------------------

library(lme4)

m_swf_lmm <- lmer(
  yield_rel ~
    distance_to_tree_strip * photo_path +
    swf_slope_0to200 +
    swf_slope_200to500 +
    swf_slope_500to1000 +
    distance_to_tree_strip * swf_slope_0to200 +
    distance_to_tree_strip * treeage +
    AFage +
    PC1_c +
    PC1_s +
    l_shdi +
    l_ed +
    (1 | field) +
    (1 | year),
  data = mod_data,
  REML = TRUE
)

AIC(m_swf_lmm)

# chatgpt suggestion ------------------------------------------------------

library(ggplot2)

newdat <- expand.grid(
  distance_to_tree_strip = seq(
    min(mod_data$distance_to_tree_strip, na.rm = TRUE),
    max(mod_data$distance_to_tree_strip, na.rm = TRUE),
    length.out = 100
  ),
  swf_slope_0to200 = seq(
    quantile(mod_data$swf_slope_0to200, 0.05, na.rm = TRUE),
    quantile(mod_data$swf_slope_0to200, 0.95, na.rm = TRUE),
    length.out = 100
  )
)

# Hold other variables at representative values
newdat$swf_slope_200to500 <- mean(mod_data$swf_slope_200to500, na.rm = TRUE)
newdat$swf_slope_500to1000 <- mean(mod_data$swf_slope_500to1000, na.rm = TRUE)
newdat$treeage <- median(mod_data$treeage, na.rm = TRUE)
newdat$AFage <- median(mod_data$AFage, na.rm = TRUE)
newdat$PC1_c <- mean(mod_data$PC1_c, na.rm = TRUE)
newdat$PC1_s <- mean(mod_data$PC1_s, na.rm = TRUE)
newdat$l_shdi <- mean(mod_data$l_shdi, na.rm = TRUE)
newdat$l_ed <- mean(mod_data$l_ed, na.rm = TRUE)

# Need a valid photo_path, field, crop and year
newdat$photo_path <- levels(factor(mod_data$photo_path))[1]
newdat$field <- levels(factor(mod_data$field))[1]
newdat$crop_unified <- levels(factor(mod_data$crop_unified))[1]
newdat$year <- levels(factor(mod_data$year))[1]

newdat$pred <- predict(
  m_swf,
  newdata = newdat,
  type = "response",
  exclude = c("s(field)", "s(crop_unified)", "s(year)")
)


ggplot(
  newdat,
  aes(
    x = distance_to_tree_strip,
    y = swf_slope_0to200,
    fill = pred
  )
) +
  geom_raster() +
  geom_contour(
    aes(z = pred),
    colour = "white",
    alpha = 0.7
  ) +
  scale_fill_viridis_c(name = "Predicted\nrelative yield") +
  labs(
    x = "Distance to tree strip (m)",
    y = "SWF slope, 0–200 m",
    title = "Interaction between SWF structure and distance to tree strip"
  ) +
  theme_minimal(base_size = 14)





# chatgpt two -------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)

swf_slopes_long <- mod_data %>%
  select(
    swf_slope_0to200,
    swf_slope_200to500,
    swf_slope_500to1000
  ) %>%
  pivot_longer(
    everything(),
    names_to = "scale",
    values_to = "slope"
  )

ggplot(swf_slopes_long, aes(x = scale, y = slope)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = NULL,
    y = "SWF slope",
    title = "SWF spatial structure at three scales"
  ) +
  theme_minimal(base_size = 14)



# gratia ------------------------------------------------------------------

summary(m_swf)$p.table



# graph 4  ----------------------------------------------------------------

coefs <- summary(m_swf)$p.table

swf_coefs <- data.frame(
  term = c(
    "SWF slope 0–200 m",
    "SWF slope 200–500 m",
    "SWF slope 500–1000 m"
  ),
  estimate = coefs[
    c(
      "swf_slope_0to200",
      "swf_slope_200to500",
      "swf_slope_500to1000"
    ),
    "Estimate"
  ],
  se = coefs[
    c(
      "swf_slope_0to200",
      "swf_slope_200to500",
      "swf_slope_500to1000"
    ),
    "Std. Error"
  ]
)

swf_coefs$lower <- swf_coefs$estimate - 1.96 * swf_coefs$se
swf_coefs$upper <- swf_coefs$estimate + 1.96 * swf_coefs$se

ggplot(swf_coefs, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15
  ) +
  geom_point(size = 3) +
  labs(
    x = "Estimated coefficient",
    y = NULL,
    title = "Estimated effects of SWF spatial slopes"
  ) +
  theme_minimal(base_size = 14)



# fitted relationship and raw data ----------------------------------------

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
    title = "Observed relationship between distance and relative yield"
  ) +
  theme_minimal(base_size = 14)
# pfr model ---------------------------------------------------------------


m_pfr_int <- pfr(
  yield_rel ~
    lf(swf_mat, argvals = swf_argvals, k = 3) +
    s(distance_to_tree_strip, k = 5) +
    ti(distance_to_tree_strip, treeage, k = c(3, 3)) +
    log_AFage +
    PC1_c +
    PC1_s +
    l_shdi +
    l_ed +
    s(field, bs = "re") +
    s(year, bs = "re"),
  data = mod_data,
  method = "REML"
)

summary(m_swf)
gam.check(m_swf)

plot(m_swf, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE, scale = 0, residuals = TRUE,
     pch = 16, cex = 0.3, col = "grey50")

AIC(m_pfr_int)
# model pfr -------------------------------------------------------------------

D <- as.numeric(scale(
  mod_data$distance_to_tree_strip,
  center = TRUE,
  scale = FALSE
))

swf_D <- swf_mat * D
termsnames <- c("swf", "swfD", "distpath", "distnear", "disttreeage", "logAFage", "climate", "soil", "shdi", "edge", "field", "year")

# with pfr i always get positive aic 
m_pfr_int <- pfr(
  yield_rel ~
    lf(swf_mat, argvals = radii, k = 3) +
    s(distance_to_tree_strip, by = photo_path, k = 3) +
  #  ti(distance_to_tree_strip, swf_mat, k = c(3, 3)) + 
    ti(distance_to_tree_strip, treeage, k = c(3, 3)) +
    log_AFage +
    PC1_c +
    PC1_s +
    l_shdi +
    l_ed +
    s(field, bs = "re") +
    s(year, bs = "re"),
  data = mod_data,
  method = "REML"
)

gam.check(m_pfr_int)
summary(m_pfr_int)
AIC(m_pfr_int)

m_pfr <- pfr(
  yield_tha ~
    lf(swf_mat, argvals = swf_argvals, k = 3) +
    s(distance_to_tree_strip, by = photo_path, k = 3) +
    ti(distance_to_tree_strip, swf_nearfield, k = c(3, 3)) + 
    ti(distance_to_tree_strip, treeage, k = c(3, 3)) +
    log_AFage +
    PC1_c +
    PC1_s +
    l_shdi +
    l_ed +
    s(field, bs = "re") +
    s(year, bs = "re"),
  data = mod_data,
  method = "REML"
)

gam.check(m_pfr)
summary(m_pfr)

AIC(m_pfr_int, m_pfr)


plot(mod_data$yield_tha ~ mod_data$AFage)
# gam ---------------------------------------------------------------------

m_gam_full <- gam(
  yield_tha ~
    s(distance_to_tree_strip, by = photo_path, k = 3) + 
    ti(distance_to_tree_strip, swf_slope,     k = c(3, 3)) +
    ti(distance_to_tree_strip, swf_nearfield, k = c(3, 3)) + 
    # ── C3/C4 × distance ──────────────────────────────────────────────────
  s(distance_to_tree_strip, by = photo_path, k = 3) +
  AFage +
  ti(distance_to_tree_strip, treeage, k = c(3, 3)) +
  PC1_c + PC1_s + l_shdi + l_ed +
  s(field, bs = "re") +
  s(year,         bs = "re"),
  data   = mod_data,   
  family = gaussian(),
  method = "REML"
)    

gam.check(m_gam_full)
summary(m_gam_full)
AIC(m_pfr_int, m_pfr, m_gam_full)


# validation and verification  --------------------------------------------

# m_pfr_int
r2_full <- summary(m_pfr_int)$r.sq

term_variance <- predict(m_pfr_int, type = "terms")
term_vars <- apply(term_variance, 2, var) # contribution of each model term to the fitted value for every observation.
total_var <- var(fitted(m_pfr_int))
colnames(term_variance)
head(term_variance)
print(sort(term_vars / total_var, decreasing = TRUE))

term_df <- data.frame(
  term = termsnames,
  variance = as.numeric(term_vars),
  proportion = as.numeric(term_vars / total_var)
)

term_df <- term_df[order(term_df$proportion, decreasing = TRUE), ]

library(ggplot2)

ggplot(term_df, aes(x = reorder(term, proportion), y = proportion)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Variance of fitted contribution / variance of fitted values"
  ) +
  theme_minimal()



summary(m_pfr_int)
term_contrib <- predict(m_pfr_int, type = "terms")

dim(term_contrib)
head(term_contrib)
str(term_contrib)



library(ggplot2)

plot_data <- data.frame(
  distance_to_tree_strip = mod_data$distance_to_tree_strip,
  swf_distance_effect = term_contrib[, 2]
)

ggplot(plot_data,
       aes(x = distance_to_tree_strip,
           y = swf_distance_effect)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Distance to tree strip",
    y = "Fitted SWF × distance contribution",
    title = "Estimated SWF × tree-strip-distance contribution"
  ) +
  theme_minimal()



plot_data <- data.frame(
  distance_to_tree_strip = mod_data$distance_to_tree_strip,
  swf_effect = term_contrib[, 1],
  swf_distance_effect = term_contrib[, 2]
)

ggplot(plot_data, aes(x = distance_to_tree_strip)) +
  geom_point(aes(y = swf_effect),
             alpha = 0.5) +
  geom_smooth(aes(y = swf_effect),
              method = "lm",
              se = TRUE) +
  labs(
    x = "Distance to tree strip",
    y = "Fitted contribution",
    title = "SWF functional effect vs. tree-strip distance"
  ) +
  theme_minimal()







library(ggplot2)

beta0 <- coef(m_pfr_int, select = 1)
beta1 <- coef(m_pfr_int, select = 2)

ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_ribbon(
    data = beta0,
    aes(
      x = swf_mat.argvals,
      ymin = value - 1.96 * se,
      ymax = value + 1.96 * se
    ),
    alpha = 0.2
  ) +
  geom_line(
    data = beta0,
    aes(x = swf_mat.argvals, y = value),
    linewidth = 1
  ) +
  geom_point(
    data = beta0,
    aes(x = swf_mat.argvals, y = value),
    size = 2
  ) +
  
  labs(
    x = "SWF radius (m)",
    y = expression(beta[0](r)),
    title = "Baseline SWF coefficient function"
  ) +
  theme_minimal()





beta0 <- coef(m_pfr_int, select = 1)
beta1 <- coef(m_pfr_int, select = 2)

distances <- c(1, 4, 7, 12, 24)

beta_surface <- expand.grid(
  radius = beta0$swf_mat.argvals,
  distance = distances
)

beta_surface$beta0 <- rep(beta0$value, times = length(distances))
beta_surface$beta1 <- rep(beta1$value, times = length(distances))

beta_surface$beta <- with(
  beta_surface,
  beta0 + distance * beta1
)



ggplot(
  beta_surface,
  aes(
    x = radius,
    y = beta,
    group = distance,
    colour = factor(distance)
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1) +
  labs(
    x = "SWF radius (m)",
    y = expression(beta(r,D)),
    colour = "Distance to tree strip (m)",
    title = "Estimated SWF coefficient by radius and tree-strip distance"
  ) +
  theme_minimal()





# BOOTSTRAPPING -----------------------------------------------------------

library(refund)
library(mgcv)
library(tidyverse)

# ── Setup ─────────────────────────────────────────────────────────────────────

n_boot       <- 1000
set.seed(42)

swf_mat_full <- swf_mat
swf_D_full   <- swf_D
argvals      <- seq(from = 100, to = 1000, by = 100)
n_obs        <- nrow(mod_data)

# ── Single bootstrap draw ─────────────────────────────────────────────────────
# Following the spider script: resample observations with replacement
# (uniform probability — every row equally likely, mirroring sample() over
# species names in the spider exercise).
# swf_mat and swf_D must be subsetted with the same indices so the functional
# predictor matrix rows stay aligned with mod_data rows.

.boot_pfr_int <- function(mod_data, swf_mat_full, swf_D_full, argvals) {
  
  idx       <- sample(seq_len(15), size = 15, replace = TRUE)
  moddatc3 <- mod_data |> filter(photo_path != "C4")
  moddatc4 <- mod_data |> filter(photo_path == "C4")
  
  boot_data <- bind_rows(moddatc3[idx, ], moddatc4)
  boot_data <- moddatc3[idx, ]
  swf_mat   <- swf_mat_full[idx, ]
  swf_D     <- swf_D_full[idx, ]
  
  fit <- tryCatch(
    refund::pfr(
      yield_tha ~
        refund::lf(swf_mat, argvals = argvals, k = 3) +
        refund::lf(swf_D,   argvals = argvals, k = 3) +
        mgcv::s(distance_to_tree_strip, by = photo_path, k = 3) +
        mgcv::ti(distance_to_tree_strip, swf_nearfield, k = c(3, 3)) +
        mgcv::ti(distance_to_tree_strip, treeage,       k = c(3, 3)) +
        log_af_age + PC1_c + PC1_s + l_shdi + l_ed +
        mgcv::s(field, bs = "re") +
        mgcv::s(year,  bs = "re"),
      data   = boot_data,
      method = "REML"
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NULL)
  
  list(
    r_sq  = summary(fit)$r.sq,
    beta0 = coef(fit, select = 1)$value,   # β₀(r) at each argval
    beta1 = coef(fit, select = 2)$value    # β₁(r) — SWF × distance
  )
}

# ── Replicate (mirrors spider: replicate(1000, sample(...) %>% unique() %>% length())) ──

boot_results <- replicate(
  n_boot,
  .boot_pfr_int(mod_data, swf_mat_full, swf_D_full, argvals),
  simplify = FALSE
)

# Remove any failed fits (convergence failures on edge-case bootstrap samples)
boot_results <- Filter(Negate(is.null), boot_results)
message("Successful fits: ", length(boot_results), " / ", n_boot)

# ── R² bootstrap distribution ─────────────────────────────────────────────────
# Mirrors: spR_girdled <- replicate(1000, sample(…) %>% unique() %>% length())
#          mean_girdled <- mean(spR_girdled)
#          mean_girdled - 1.96 * sd_girdled

r_sq_boot  <- sapply(boot_results, `[[`, "r_sq")

mean_r_sq  <- mean(r_sq_boot)
sd_r_sq    <- sd(r_sq_boot)
ci_r_sq    <- c(lower = mean_r_sq - 1.96 * sd_r_sq,
                upper = mean_r_sq + 1.96 * sd_r_sq)

# ── Pointwise bootstrap CIs for β₀(r) and β₁(r) ─────────────────────────────

beta0_mat <- do.call(rbind, lapply(boot_results, `[[`, "beta0"))  # n_boot × 10
beta1_mat <- do.call(rbind, lapply(boot_results, `[[`, "beta1"))

make_ci_df <- function(boot_mat, argvals) {
  data.frame(
    radius = argvals,
    mean   = colMeans(boot_mat),
    sd     = apply(boot_mat, 2, sd)
  ) |>
    dplyr::mutate(
      lower = mean - 1.96 * sd,
      upper = mean + 1.96 * sd
    )
}

beta0_ci <- make_ci_df(beta0_mat, argvals)
beta1_ci <- make_ci_df(beta1_mat, argvals)

# Observed (full-data) coefficient functions for overlay
obs_beta0 <- coef(m_pfr_int, select = 1)
obs_beta1 <- coef(m_pfr_int, select = 2)

obs_beta0_df <- data.frame(
  radius = obs_beta0[[grep("\\.argvals$", names(obs_beta0))]],
  value  = obs_beta0$value
)
obs_beta1_df <- data.frame(
  radius = obs_beta1[[grep("\\.argvals$", names(obs_beta1))]],
  value  = obs_beta1$value
)

# ── Plot 1: R² bootstrap histogram ───────────────────────────────────────────

p_boot_r2 <- ggplot(data.frame(r_sq = r_sq_boot), aes(x = r_sq)) +
  geom_histogram(bins = 40, fill = "#4D7FA3", colour = "white", alpha = 0.85) +
  geom_vline(xintercept = summary(m_pfr_int)$r.sq,
             colour = "firebrick", linetype = "dashed", linewidth = 1,
             show.legend = TRUE) +
  geom_vline(xintercept = ci_r_sq,
             colour = "grey30", linetype = "dotted", linewidth = 0.8) +
  labs(
    x        = expression(R^2),
    y        = "Bootstrap count",
    title    = expression("Bootstrap distribution of " * R^2 * " — m_pfr_int"),
    subtitle = paste0(
      "n = ", length(boot_results),
      "  |  mean = ", round(mean_r_sq, 3),
      "  |  95 % CI [", round(ci_r_sq["lower"], 3),
      ", ", round(ci_r_sq["upper"], 3), "]"
    ),
    caption = "Dashed red = observed R²  |  Dotted grey = 95 % bootstrap CI"
  ) +
  theme_minimal(base_size = 12)

p_boot_r2

# ── Plot 2: β₀(r) — bootstrap CI vs. observed ────────────────────────────────

p_boot_beta0 <- ggplot(beta0_ci, aes(x = radius)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  # bootstrap mean ± 1.96 SD band
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#4D7FA3", alpha = 0.25) +
  geom_line(aes(y = mean), colour = "#4D7FA3", linewidth = 0.9,
            linetype = "longdash") +
  # observed full-data estimate
  geom_line(data = obs_beta0_df, aes(y = value),
            colour = "#1B4F72", linewidth = 1.1) +
  geom_point(data = obs_beta0_df, aes(y = value),
             colour = "#1B4F72", size = 2.5) +
  scale_x_continuous(breaks = seq(100, 1000, by = 100)) +
  labs(
    x        = "SWF radius (m)",
    y        = expression(beta[0](r)),
    title    = expression("Bootstrap CIs for baseline SWF coefficient " * beta[0](r)),
    subtitle = paste0("n = ", length(boot_results),
                      " replicates  |  band = bootstrap mean ± 1.96 SD  |  solid = observed")
  ) +
  theme_minimal(base_size = 12)

p_boot_beta0

# ── Plot 3: β₁(r) — bootstrap CI vs. observed ────────────────────────────────

p_boot_beta1 <- ggplot(beta1_ci, aes(x = radius)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#A85D5D", alpha = 0.25) +
  geom_line(aes(y = mean), colour = "#A85D5D", linewidth = 0.9,
            linetype = "longdash") +
  geom_line(data = obs_beta1_df, aes(y = value),
            colour = "#6B2020", linewidth = 1.1) +
  geom_point(data = obs_beta1_df, aes(y = value),
             colour = "#6B2020", size = 2.5) +
  scale_x_continuous(breaks = seq(100, 1000, by = 100)) +
  labs(
    x        = "SWF radius (m)",
    y        = expression(beta[1](r)),
    title    = expression("Bootstrap CIs for interaction term " * beta[1](r) * " (SWF × distance)"),
    subtitle = paste0("n = ", length(boot_results),
                      " replicates  |  band = bootstrap mean ± 1.96 SD  |  solid = observed")
  ) +
  theme_minimal(base_size = 12)

p_boot_beta1

# ── Plot 4: β₀(r) vs β₁(r) — overlaid for comparison ────────────────────────

both_ci <- bind_rows(
  mutate(beta0_ci, term = "\u03b2\u2080(r) — baseline SWF"),
  mutate(beta1_ci, term = "\u03b2\u2081(r) — SWF \u00d7 distance")
)

both_obs <- bind_rows(
  mutate(obs_beta0_df, term = "\u03b2\u2080(r) — baseline SWF"),
  mutate(obs_beta1_df, term = "\u03b2\u2081(r) — SWF \u00d7 distance")
)

p_boot_both <- ggplot(both_ci, aes(x = radius, colour = term, fill = term)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, colour = NA) +
  geom_line(aes(y = mean), linetype = "longdash", linewidth = 0.9) +
  geom_line(data = both_obs,  aes(y = value), linewidth = 1.1) +
  geom_point(data = both_obs, aes(y = value), size = 2) +
  scale_colour_manual(values = c("#1B4F72", "#6B2020")) +
  scale_fill_manual(values   = c("#4D7FA3", "#A85D5D")) +
  scale_x_continuous(breaks = seq(100, 1000, by = 100)) +
  labs(
    x        = "SWF radius (m)",
    y        = "Coefficient value",
    colour   = NULL, fill = NULL,
    title    = "Bootstrap CIs for both SWF coefficient functions",
    subtitle = paste0("n = ", length(boot_results),
                      "  |  dashed = bootstrap mean  |  solid = observed")
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p_boot_both

# ── Summary table ─────────────────────────────────────────────────────────────

boot_summary <- data.frame(
  statistic = c("R²"),
  observed  = round(summary(m_pfr_int)$r.sq, 3),
  boot_mean = round(mean_r_sq, 3),
  boot_sd   = round(sd_r_sq,   3),
  ci_lower  = round(ci_r_sq["lower"], 3),
  ci_upper  = round(ci_r_sq["upper"], 3)
)

print(boot_summary)
