rm(list=ls())

# ============================================================================
# yield_tha ~ surrounding landscape + intrinsic variation + field design
# Response (yield) is scalar per  -> scalar-on-function regression
# via refund::pfr
# ============================================================================

library(dplyr)
library(tidyr)
library(refund)
library(mgcv)

df <- read.csv("./data/AnalysisData/20260819_AFswf.csv")


# ---- Year Planting ---- -------------------------------------------------


df <- df |> 
  mutate(year_AFplanting = case_when(
    field == "Dornburg" ~ 2007, 
    field == "Forst" ~ 2010, 
    field == "Gladbacherhof" ~ 2020, 
    field == "IhingerHof" ~ 2008, 
    field == "Mariensee" ~ 2008, 
    field == "Reiffenhausen" ~ 2011, 
    field == "Vechta" ~ 2019, 
    field == "Wendhausen" ~ 2008
  ) )

summary(df$year_AFplanting)

# ---- Fertilization Rate ---- -------------------------------------------------

fert_raw <- gsub("[\u2013\u2212]", "-", df$fertilization_rate_kg_n_p_k_ha_1_year_1)
df$fert_N <- as.numeric(sub("^([0-9.]+).*", "\\1", fert_raw))
df$fert_N[fert_raw == "no fertilization"] <- 0

summary(df$fert_N)

# ============================================================================
# (1) SURROUNDING LANDSCAPE: functional covariate prop_swf(distance)
#     -> interpolate onto a common distance grid per id (radii are irregular)
# ============================================================================

W_swf <- df |>
  dplyr::select(id, distance, swf_year, prop_swf) |> 
  unique() |> 
  pivot_wider(names_from = distance, values_from = prop_swf) |>
  arrange(id)

id_order <- W_swf$id
W_mat    <- as.matrix(W_swf[, -1])

# ============================================================================
# (2) + (3) base data: one row per id, intrinsic variation + field design
# ============================================================================

base <- df |>
  distinct(id, field, year, yield_tha, distance_to_tree_strip,
           crop_unified, area, fert_N, temp_C_mean, sun_MJ_m2_mean,
           precip_mm_sum, l_shdi, l_contag, l_ed, l_ai,
           mean_slope, min_slope, max_slope, treeage, year_AFplanting, prop_swf_within) |>
  filter(id %in% id_order) |>
  slice(match(id_order, id))          # align rows with W_mat exactly

stopifnot(nrow(base) == nrow(W_mat))
base$W_swf <- W_mat

base$field        <- factor(base$field)
base$year_AFplanting         <- factor(base$year_AFplanting)
base$treeage         <- factor(base$treeage)
base$crop_unified <- factor(base$crop_unified)
base$harvestyear  <- factor(base$harvestyear)

# ============================================================================
# MODEL: scalar-on-function regression
# ============================================================================

# safe_k(): never let a smooth's k exceed (unique values - 1) of its covariate,
# so smooth.construct never errors regardless of how listwise deletion
# (NAs in any term) thins the fitting sample
safe_k <- function(x, k_wanted) {
  min(k_wanted, length(unique(x[!is.na(x)])) - 1)
}

k_dist  <- safe_k(base$distance_to_tree_strip, 6)
k_fertN <- safe_k(base$fert_N,                 5)
k_temp  <- safe_k(base$temp_C_mean,            5)
k_sun   <- safe_k(base$sun_MJ_m2_mean,         5)
k_prec  <- safe_k(base$precip_mm_sum,          5)
k_shdi  <- safe_k(base$l_shdi,                 5)
k_cont  <- safe_k(base$l_contag,               5)
k_ed    <- safe_k(base$l_ed,                   5)
k_ai    <- safe_k(base$l_ai,                   5)
k_tree  <- safe_k(base$treeage,                5)

model <- pfr(
  yield_tha ~
    lf(W_swf, argvals = grid, bs = "ps", k = 15) +                     # (1) surrounding landscape
    s(distance_to_tree_strip, bs = "tp", k = k_dist) +                  # (2) intrinsic variation
    # area, mean_slope dropped: each has exactly 1 unique value per field ->
    # fully confounded with s(field, bs="re"); keep as descriptive, not modeled
    s(fert_N,         bs = "tp", k = k_fertN) +                          # (3) field design
    s(temp_C_mean,    bs = "tp", k = k_temp) +
    s(sun_MJ_m2_mean, bs = "tp", k = k_sun) +
    s(precip_mm_sum,  bs = "tp", k = k_prec) +
    s(l_shdi,         bs = "tp", k = k_shdi) +
    s(l_contag,       bs = "tp", k = k_cont) +
    s(l_ed,           bs = "tp", k = k_ed) +
    s(l_ai,           bs = "tp", k = k_ai) +
    s(treeage,        bs = "tp", k = k_tree) +
    crop_unified +
    harvestyear +
    s(field, bs = "re") +
    s(plot,  bs = "re"),
  data   = base,
  method = "REML"
)

summary(model)

plot(model, select = 1, shade = TRUE)   # coefficient function beta(distance) for prop_swf
plot(model, pages = 1, scale = 0)       # all smooth terms

