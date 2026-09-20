

library(refund)   
library(mgcv)    
library(tidyverse)
library(gratia)    


df <- read_csv("01_Data/AF_swf.csv")  


dflong <- df  |> 
  group_by(field, year, crop_unified, distance_to_tree_strip) |>
  summarise(
    yield_tha     = mean(yield_tha, na.rm = TRUE),
    fert_N        = mean(fert_N), # fertilimod_dataation
    temp_C_mean   = first(temp_C_mean), # climate
    precip_mm_sum = first(precip_mm_sum), # climate
    sun_MJ_m2_mean= first(sun_MJ_m2_mean), # climate
    AFage         = first(AFage), # AFdesign
    treeage         = first(treeage), # AFdesign
    clay          = first(clay), #soil
    sand          = first(sand), #soil
    silt          = first(silt), #soil
    l_shdi        = first(l_shdi), # landscape
    l_ed          = first(l_ed), # landscpae
    l_np          = first(l_np), # landscape
    l_contag      = first(l_contag), # landscape
    mean_slope    = first(mean_slope), # slope
    .groups = "drop"
  )  |> 
  mutate(
    photo_path   = factor(ifelse(crop_unified == "maimod_datae", "C4", "C3")),
    log_AFage    = log1p(AFage),
    log_dist     = log(distance_to_tree_strip),
    crop_unified = factor(crop_unified),
    field        = factor(field),
    year         = factor(year),
    dist_bin     = factor(distance_to_tree_strip),
    # yield relative to unit mean (for interpretation)
    .by = c(field, year, crop_unified)
  ) |>
  group_by(field, year, crop_unified) |>
  mutate(yield_rel = yield_tha / mean(yield_tha)) |>
  ungroup()


nrow(dflong) # 140
nrow(distinct(dflong, field, year, crop_unified)) # 34
table(dflong$photo_path) # C4: 15, C3: 125


df |>
  filter(!is.na(prop_swf)) |>
  select(field, year, distance, prop_swf) |> unique()


swf_unit <- df |>
  filter(!is.na(prop_swf)) |>
  group_by(field, year, distance) |>
  summarise(prop_swf = mean(prop_swf, na.rm = TRUE), .groups = "drop") |>
  group_by(field, year) |>
  arrange(distance, .by_group = TRUE) |>
  mutate(radius_rank = row_number()) |>
  ungroup()

swf_summary <- swf_unit |>
  group_by(field, year) |>
  summarise(
    swf_mean      = mean(prop_swf),
    swf_max       = max(prop_swf),
    swf_nearfield = first(prop_swf[distance == min(distance)]),  # SWF at smallest distance
    swf_slope     = coef(lm(prop_swf ~ distance))[["distance"]], # linear slope
    swf_n_radii   = n(),
    .groups = "drop"
  ) |> mutate(year = as.factor(year))

swf_wide <- swf_unit |>
  pivot_wider(
    id_cols    = c(field, year),
    names_from = distance,
    names_prefix = "swf_d",
    values_from = prop_swf
  ) |> mutate(year = as.factor(year))

mod_data <- dflong |>
  left_join(swf_summary, by = c("field", "year")) |>
  left_join(swf_wide,    by = c("field", "year")) |> 
  mutate(field = as.factor(field),
         year = as.factor(year))

nrow(mod_data) # 140



# principal components ----------------------------------------------------
field        <- as.factor(mod_data$field)
year         <- as.factor(mod_data$year)

temp       <- as.numeric(mod_data$temp_C_mean)
sun        <- as.numeric(mod_data$sun_MJ_m2_mean)
precip     <- as.numeric(mod_data$precip_mm_sum)
clay       <- as.numeric(mod_data$clay)
sand       <- as.numeric(mod_data$sand)
silt       <- as.numeric(mod_data$silt)

climate   <- data.frame(field, year, temp, sun, precip)
climate_s <- scale(climate[-c(1:2)])
data.pca_c  <- princomp(climate_s)
soil   <- data.frame(field, year, clay, sand, silt)
soil_s <- scale(soil[-c(1:2)])
data.pca_s <- princomp(soil_s)
soil_pca      <- data.frame(year = soil$year,  field = soil$field,      PC1_s = data.pca_s$scores[, 1]) |> unique()
climate_pca   <- data.frame(year = climate$year,field = soil$field,     PC1_c = data.pca_c$scores[, 1])|> unique()

mod_data <- mod_data |>
  dplyr::left_join(soil_pca, by = c("field", "year")) |>
  dplyr::left_join(climate_pca, by = c("field", "year"))|> 
  mutate(field = as.factor(field),
         year = as.factor(year))

# distance vector ─────────────────────────────────────
swf_argvals <- seq(from = 100, to = 1000, by=100)

# ── 0d. SWF matrix aligned to mod_data rows ───────────────────────────
# swf matrix
swf_mat <- mod_data |>
  select(starts_with("swf_d")) |>
  as.matrix()

summary(swf_mat)


# write csv ---------------------------------------------------------------

write.csv(mod_data, "01_Data/20260920_moddata.csv", row.names = FALSE)
write.csv(swf_mat, "01_Data/20260920_swf_mat.csv", row.names = FALSE)

# model pfr -------------------------------------------------------------------

D <- as.numeric(scale(
  mod_data$distance_to_tree_strip,
  center = TRUE,
  scale = FALSE
))

swf_D <- swf_mat * D
termsnames <- c("swf", "swfD", "distpath", "distnear", "disttreeage", "logAFage", "climate", "soil", "shdi", "edge", "field", "year")



m_pfr_int <- pfr(
  yield_tha ~
    lf(swf_mat, argvals = swf_argvals, k = 3) +
    lf(swf_D,   argvals = swf_argvals, k = 3) +
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
