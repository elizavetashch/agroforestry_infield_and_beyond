

# =============================================================================
# Agroforestry Yield Analysis — audited & cleaned script
# Audit fixes applied:
#   [F1] All libraries moved to top (refund, gratia were mid-script)
#   [F2] m0_pfr moved AFTER swf_matrix/swf_argvals are built
#   [F3] r2() replaced with summary()$r.sq; duplicate assignment fixed
#   [F4] fert_N extracted alongside other numeric predictors
#   [F5] gitcreds::gitcreds_set() removed
#   [S1] te() argument order unified to te(lat, long, year_num) throughout
#   [S2] Random-slope comment added to s(crop_unified, distance_to_tree_strip)
#   [S3] library() calls de-duplicated and grouped by purpose
# =============================================================================


# Libraries ---------------------------------------------------------------
# [F1] All packages loaded here — refund and gratia were previously loaded
#      mid-script, which causes errors if those sections are sourced in order.

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

# Reporting
library(knitr)


# Palette -----------------------------------------------------------------
fieldpalette <- c(
  "#577590", "#4d908e", "#43aa8b", "#90be6d",
  "#f9c74f", "#f8961e", "#f3722c", "#f94144"
)


# Load & prepare data -----------------------------------------------------
df <- read.csv("01_Data/dffinal_20260909.csv")

df <- df |>
  mutate(
    yield_log     = log(yield_tha + 2),
    yield_scaled  = as.numeric(scale(yield_log)),
    distance_log  = log(distance_to_tree_strip),
    field         = as.factor(field),
    year          = as.factor(year),
    crop_unified  = as.factor(crop_unified)
  )

table(df$field)
colnames(df)


# Distinct row sets -------------------------------------------------------
# Field × year level (34 rows expected)
o <- df |> distinct(
  field, year, fieldlength, l_contag, l_sidi,
  l_shdi, l_np, l_ai, l_ed, swf_year, prop_swf_within,
  latitude, longitude, temp_C_mean, sun_MJ_m2_mean,
  precip_mm_sum, min_slope, mean_slope, max_slope,
  clay, sand, silt, treeage, AFage
)
nrow(o)  # expected 34

# Yield rows
y   <- df |> distinct(field, year, crop_unified, distance_to_tree_strip, yield_tha)
x   <- df |> distinct(field, year, crop_unified, distance_to_tree_strip, yield_tha, ID_nodist)
xs  <- df |> distinct(field, year, crop_unified, distance_to_tree_strip, yield_tha, ID_nodist, swf_year)

z <- df |>
  select(-c("radius", "distance", "radius.x", "radius.y", "prop_swf", "swf_year")) |>
  distinct()

nrow(xs)  # expected 1171
nrow(y)   # expected 1158
nrow(x)   # expected 1171
nrow(z)   # expected 1171

plot(x$yield_tha ~ x$distance_to_tree_strip)
# functional relationship visible

# Continuous SWF predictor
s <- df |> distinct(field, swf_year, prop_swf, distance, id)
nrow(s)  # expected 180

plot(s$prop_swf ~ s$distance)
# also a functional relationship


# PCA — climate -----------------------------------------------------------
# Factor levels
year         <- as.factor(z$year)
id           <- as.factor(z$id)
field        <- as.factor(z$field)
year_treecut <- as.factor(z$harvestyear)
crop         <- as.factor(z$crop_unified)

# Numeric predictors
lat        <- as.numeric(z$latitude)
long       <- as.numeric(z$longitude)
temp       <- as.numeric(z$temp_C_mean)
sun        <- as.numeric(z$sun_MJ_m2_mean)
precip     <- as.numeric(z$precip_mm_sum)
min_slope  <- as.numeric(z$min_slope)
mean_slope <- as.numeric(z$mean_slope)
max_slope  <- as.numeric(z$max_slope)
clay       <- as.numeric(z$clay)
sand       <- as.numeric(z$sand)
silt       <- as.numeric(z$silt)
fert_N     <- as.numeric(z$fert_N)   # [F4] was missing; used in m0_f, m4, m5

climate   <- data.frame(field, id, temp, sun, precip)
climate_s <- scale(climate[-c(1:2)])

corr_matrix_c <- cor(climate_s)
ggcorrplot(corr_matrix_c, hc.order = TRUE, type = "lower", lab = TRUE)

data.pca_c  <- princomp(climate_s)
(pca_sum_c  <- summary(data.pca_c))

fviz_pca_ind(
  data.pca_c,
  label      = FALSE,
  col.ind    = as.factor(climate$field),
  palette    = fieldpalette,
  pointshape = 16,
  pointsize  = 3.5,
  title      = "Climate PCA — temperature, precipitation, solar radiation"
) +
  theme_classic() +
  theme(legend.title = element_blank())

fviz_cos2(data.pca_c, choice = "var", axes = 1:2)


# PCA — soil --------------------------------------------------------------
soil   <- data.frame(field, id, clay, sand, silt)
soil_s <- scale(soil[-c(1:2)])

corr_matrix_s <- cor(soil_s)
ggcorrplot(corr_matrix_s, hc.order = TRUE, type = "lower", lab = TRUE)

data.pca_s <- princomp(soil_s)
(pca_sum_s <- summary(data.pca_s))

fviz_pca_ind(
  data.pca_s,
  label      = FALSE,
  col.ind    = as.factor(soil$field),
  palette    = fieldpalette,
  pointshape = 16,
  pointsize  = 3.5,
  title      = "Soil PCA — soil texture"
) +
  theme_classic() +
  theme(legend.title = element_blank())

fviz_cos2(data.pca_s, choice = "var", axes = 1:2)


# PCA — landscape ---------------------------------------------------------
fieldlength  <- as.numeric(z$fieldlength)
contag       <- as.numeric(z$l_contag)
simpson      <- as.numeric(z$l_sidi)
shannon      <- as.numeric(z$l_shdi)
patchnumber  <- as.numeric(z$l_np)
aggregation  <- as.numeric(z$l_ai)
edgedensity  <- as.numeric(z$l_ed)

landscape   <- data.frame(field, id, contag, simpson, shannon,
                          patchnumber, aggregation, edgedensity)
landscape_s <- scale(landscape[-c(1:2)])

corr_matrix_l <- cor(landscape_s)
ggcorrplot(corr_matrix_l, hc.order = TRUE, type = "lower", lab = TRUE)

data.pca_l <- princomp(landscape_s)
(pca_sum_l <- summary(data.pca_l))

fviz_pca_ind(
  data.pca_l,
  label      = FALSE,
  col.ind    = as.factor(landscape$field),
  palette    = fieldpalette,
  pointshape = 16,
  pointsize  = 3.5,
  title      = "Landscape PCA — all landscape indices"
) +
  theme_classic() +
  theme(legend.title = element_blank())

fviz_cos2(data.pca_l, choice = "var", axes = 1:2)


# PCA scores → main data frame --------------------------------------------
landscape_pca <- data.frame(id = landscape$id, PC1_l = data.pca_l$scores[, 1])
soil_pca      <- data.frame(id = soil$id,       PC1_s = data.pca_s$scores[, 1])
climate_pca   <- data.frame(id = climate$id,    PC1_c = data.pca_c$scores[, 1])

z <- z |>
  dplyr::left_join(landscape_pca, by = "id") |>
  dplyr::left_join(soil_pca,      by = "id") |>
  dplyr::left_join(climate_pca,   by = "id") |>
  mutate(year_num = as.numeric(year))

plot(z$yield_log ~ z$distance_log)


# Functional (SWF) data preparation --------------------------------------
# Must be built before any pfr() model calls.
# [F2] m0_pfr was previously placed before this block — now moved below.

s_clean <- s |>
  mutate(year = as.integer(str_extract(swf_year, "\\d{4}"))) |>
  select(field, year, distance, prop_swf, id)

swf_wide <- s_clean |>
  pivot_wider(
    names_from  = distance,
    values_from = prop_swf,
    names_prefix = "d_"
  ) |>
  arrange(id)

# Distance grid (argument values of the functional predictor)
swf_argvals <- sort(unique(s_clean$distance))  # e.g. 100, 200, ..., 1000

swf_cols <- paste0("d_", swf_argvals)

z_swf <- z |>
  left_join(swf_wide, by = "id")

# Matrix: rows = observations, cols = SWF distance grid
swf_matrix <- as.matrix(z_swf[, swf_cols])

dim(swf_matrix)         # should be nrow(z_swf) × length(swf_argvals)
sum(is.na(swf_matrix))  # 0 if join is complete

# Interaction matrices used in pfr models
swf_x_prop      <- swf_matrix * z_swf$prop_swf_within
swf_x_dist      <- swf_matrix * log(z_swf$distance_to_tree_strip)
swf_x_prop_dist <- swf_matrix * z_swf$prop_swf_within * log(z_swf$distance_to_tree_strip)


# Models ------------------------------------------------------------------

# m0 — baseline (no distance) --------------------------------------------
m0 <- gam(
  yield_log ~
    PC1_c + PC1_l + PC1_s +
    mean_slope + treeage + AFage +
    s(crop_unified, bs = "re") +
    te(lat, long, year_num),   # [S1] unified argument order
  data   = z,
  method = "REML"
)
summary(m0)


# m0_dist — baseline + log distance --------------------------------------
m0_dist <- gam(
  yield_log ~
    s(log(distance_to_tree_strip), k = 3) +
    PC1_c + PC1_l + PC1_s +
    mean_slope + treeage + AFage +
    # [S2] s(crop, dist, bs="re") = random slope of distance per crop level
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    te(lat, long, year_num),
  data   = z,
  method = "REML"
)
summary(m0_dist)
AIC(m0_dist)


# m0_pfr — baseline + functional SWF predictor ---------------------------
# [F2] Moved here (after swf_matrix and swf_argvals are defined)
m0_pfr <- pfr(
  yield_log ~
    lf(swf_matrix, argvals = swf_argvals, k = 3) +
    s(log(distance_to_tree_strip), k = 3) +
    PC1_c + PC1_l + PC1_s +
    mean_slope + treeage + AFage +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    te(lat, long, year_num),
  data   = z_swf,
  method = "REML"
)
summary(m0_pfr)
AIC(m0_pfr)


# m0_f — baseline + fertiliser N -----------------------------------------
m0_f <- gam(
  yield_log ~
    PC1_c + PC1_l + PC1_s + fert_N +
    mean_slope + treeage + AFage +
    s(crop_unified, bs = "re") +
    te(lat, long, year_num),
  data   = z,
  method = "REML"
)
summary(m0_f)
plot(m0_f, residuals = TRUE)
gam.check(m0_f)
AIC(m0_f)


# m1 — distance + simple random effects ----------------------------------
m1 <- gam(
  yield_log ~
    s(log(distance_to_tree_strip), k = 3) +
    s(crop_unified, bs = "re") +
    s(field, bs = "re") +
    s(year,  bs = "re"),
  data   = z,
  method = "REML"
)
summary(m1)
plot(m1, residuals = TRUE)
gam.check(m1)


# m1_crop — distance + random slope per crop -----------------------------
m1_crop <- gam(
  yield_log ~
    s(log(distance_to_tree_strip), k = 3) +
    s(crop_unified, distance_to_tree_strip, bs = "re") +  # [S2] random slope
    s(field, bs = "re") +
    s(year,  bs = "re"),
  data   = z,
  method = "REML"
)
summary(m1_crop)
plot(m1_crop, residuals = TRUE)
gam.check(m1_crop)


# m2 — PCA component models ----------------------------------------------
m2_pcl <- gam(
  yield_log ~
    s(log(distance_to_tree_strip), k = 3) + PC1_l +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") + s(year, bs = "re"),
  data = z, method = "REML"
)

m2_pcs <- gam(
  yield_log ~
    s(log(distance_to_tree_strip), k = 3) + PC1_s +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") + s(year, bs = "re"),
  data = z, method = "REML"
)

m2_pcc <- gam(
  yield_log ~
    s(log(distance_to_tree_strip), k = 3) + PC1_c +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") + s(year, bs = "re"),
  data = z, method = "REML"
)

m2_pcsc <- gam(
  yield_log ~
    s(log(distance_to_tree_strip), k = 3) + PC1_s + PC1_c +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") + s(year, bs = "re"),
  data = z, method = "REML"
)

m2_pcscl <- gam(
  yield_log ~
    s(log(distance_to_tree_strip), k = 3) + PC1_s + PC1_l + PC1_c +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") + s(year, bs = "re"),
  data = z, method = "REML"
)

summary(m2_pcl)
summary(m2_pcs)
summary(m2_pcc)
summary(m2_pcsc)
summary(m2_pcscl)

plot(m2_pcsc,  residuals = TRUE); gam.check(m2_pcsc)
plot(m2_pcscl, residuals = TRUE); gam.check(m2_pcscl)
plot(m2_pcl,   residuals = TRUE); gam.check(m2_pcl)
gam.check(m2_pcs)
gam.check(m2_pcc)

AIC(m2_pcl, m2_pcs, m2_pcc, m2_pcsc, m2_pcscl)


# m3 — landscape / SWF indices -------------------------------------------
m3_shannon <- gam(
  yield_log ~
    s(log(distance_to_tree_strip), k = 3) + s(l_shdi) + PC1_s + PC1_c +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") + s(year, bs = "re"),
  data   = z,
  method = "REML"
)
summary(m3_shannon)
AIC(m3_shannon)
gam.check(m3_shannon)
par(mfrow = c(2, 2)); plot(m3_shannon, residuals = TRUE); par(mfrow = c(1, 1))

# Dispersion check (should be ~1 for Gaussian)
dispersion <- sum(residuals(m3_shannon, type = "pearson")^2) / df.residual(m3_shannon)
dispersion

m3_swffield <- gam(
  yield_log ~
    s(log(distance_to_tree_strip), k = 3) + s(prop_swf_within) + s(l_shdi) +
    PC1_s + PC1_c +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") + s(year, bs = "re"),
  data   = z,
  method = "REML"
)
summary(m3_swffield)
gam.check(m3_swffield)
par(mfrow = c(2, 2)); plot(m3_swffield, residuals = TRUE); par(mfrow = c(1, 1))


# m3_swf_functional — functional SWF effect (pfr) ------------------------
m3_swf_functional <- pfr(
  yield_log ~
    lf(swf_matrix, argvals = swf_argvals, k = 8) +
    s(log(distance_to_tree_strip), k = 3) +
    s(l_shdi) +
    PC1_s + PC1_c +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") +
    s(year,  bs = "re"),
  data   = z_swf,
  method = "REML"
)
summary(m3_swf_functional)
#plot(m3_swf_functional, select = 1)  # coefficient function β(distance from border)


# m4 — spatio-temporal + fertiliser -------------------------------------
m4 <- gam(
  yield_log ~
    s(log(distance_to_tree_strip), k = 3) + s(prop_swf_within) +
    PC1_l + PC1_s + PC1_c + s(fert_N) +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    te(lat, long, year_num),   # [S1] unified argument order
  data   = z,
  method = "REML"
)
summary(m4)
gam.check(m4)
par(mfrow = c(1, 1)); plot(m4, residuals = TRUE)

# [F3] r2() is not a base/mgcv function — extract from summary directly.
#      Also fixed duplicate assignment (m4_sl = m4_pcl was a copy-paste error).
m4_r2_adj   <- summary(m4)$r.sq        # adjusted R²
m4_dev_expl <- summary(m4)$dev.expl    # deviance explained


# Functional interaction models (pfr) ------------------------------------
m_int1 <- pfr(
  yield_log ~
    lf(swf_matrix,  argvals = swf_argvals, k = 8) +
    lf(swf_x_prop,  argvals = swf_argvals, k = 8) +
    s(log(distance_to_tree_strip), k = 3) +
    s(l_shdi) +
    PC1_s + PC1_c +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") +
    s(year,  bs = "re"),
  data   = z_swf,
  method = "REML"
)
summary(m_int1)
# draw(m_int1) # doesnt work

m_int2 <- pfr(
  yield_log ~
    lf(swf_matrix,      argvals = swf_argvals, k = 8) +
    lf(swf_x_prop,      argvals = swf_argvals, k = 8) +
    lf(swf_x_prop_dist, argvals = swf_argvals, k = 8) +
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


# m5 — full functional monster model -------------------------------------
m5 <- pfr(
  yield_log ~
    lf(swf_matrix,      argvals = swf_argvals, k = 8) +
    lf(swf_x_prop,      argvals = swf_argvals, k = 8) +
    lf(swf_x_prop_dist, argvals = swf_argvals, k = 8) +
    s(log(distance_to_tree_strip), k = 3) +
    s(prop_swf_within) +
    PC1_l +
    s(fert_N) +
    PC1_s + PC1_c +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") +
    s(year,  bs = "re"),
  data   = z_swf,
  method = "REML"
)
summary(m5)


# Model comparison --------------------------------------------------------

# Helper: tidy metrics from any gam/pfr object
gam_metrics <- function(model, label) {
  s <- summary(model)
  tibble(
    Model        = label,
    AIC          = round(AIC(model), 2),
    BIC          = round(BIC(model), 2),
    Dev_Expl_pct = round(s$dev.expl * 100, 1),
    R2_adj       = round(s$r.sq, 3),
    n            = nobs(model)
  )
}

all_models <- list(
  "m0 (baseline, no distance)"           = m0,
  "m0_dist (+ log distance)"             = m0_dist,
  "m0_pfr (+ functional SWF)"            = m0_pfr,
  "m0_f (+ fert_N)"                      = m0_f,
  "m1 (distance + RE)"                   = m1,
  "m1_crop (random slope crop)"          = m1_crop,
  "m2_pcl (+ PC1 landscape)"            = m2_pcl,
  "m2_pcs (+ PC1 soil)"                 = m2_pcs,
  "m2_pcc (+ PC1 climate)"              = m2_pcc,
  "m2_pcsc (soil + climate)"            = m2_pcsc,
  "m2_pcscl (soil + climate + land)"    = m2_pcscl,
  "m3_shannon (+ SHDI)"                 = m3_shannon,
  "m3_swffield (+ prop_swf_within)"     = m3_swffield,
  "m3_swf_functional (lf SWF)"          = m3_swf_functional,
  "m4 (+ fert_N + spatio-temporal)"     = m4,
  "m_int1 (lf SWF + lf SWF×prop)"      = m_int1,
  "m_int2 (+ lf SWF×prop×dist)"        = m_int2,
  "m5 (full functional monster)"        = m5
)

comparison_tbl <- bind_rows(
  mapply(gam_metrics,
         model  = all_models,
         label  = names(all_models),
         SIMPLIFY = FALSE)
) |>
  arrange(AIC) |>
  mutate(delta_AIC = round(AIC - min(AIC), 2))

kable(comparison_tbl,
      caption = "Model comparison — ordered by AIC (lower = better fit)",
      align   = c("l", "r", "r", "r", "r", "r", "r"))

# AIC dot plot
ggplot(comparison_tbl,
       aes(x = AIC, y = reorder(Model, -AIC))) +
  geom_segment(aes(xend = min(AIC), yend = reorder(Model, -AIC)),
               colour = "grey70", linewidth = 0.4) +
  geom_point(aes(colour = Dev_Expl_pct), size = 4) +
  scale_colour_gradient(low  = "#577590", high = "#f94144",
                        name = "Deviance\nexplained (%)") +
  labs(
    title    = "Model comparison by AIC",
    subtitle = "Shorter bar = better fit   |   colour = deviance explained",
    x        = "AIC",
    y        = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(panel.grid.major.x = element_line(colour = "grey90"))

# Pairwise AIC for key candidate models only
AIC(m1_crop, m2_pcsc, m3_shannon,
    m3_swf_functional, m_int2, m5) |>
  as.data.frame() |>
  rownames_to_column("Model") |>
  mutate(
    Model     = c("m1_crop", "m2_pcsc", "m3_shannon",
                  "m3_swf_functional", "m_int2", "m5"),
    delta_AIC = round(AIC - min(AIC), 2)
  ) |>
  arrange(AIC) |>
  kable(caption = "Key candidate models — AIC comparison", digits = 2)

# [F5] gitcreds::gitcreds_set() removed — was a stray interactive call
#      that would pause or break any sourced run.