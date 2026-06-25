
# Clean teh Environment
rm(list=ls())

library(dplyr)
df <-  rdf <-  rdf <-  read.csv("data/AnalysisData/20260618_df.csv")

names(df)

# Check coordinates ----------------------------------------
df[is.na(df$lat), ] # clean, 0 rows


# Year Distribution -------------------------------------------------------

hist(df$year)

# Column Names -------------------------------------------------------

sink("data/AnalysisData/dataset_names.txt")

cat("=========================================================\n")
cat("ANALYSIS DATASET NAMES - Generated:", format(Sys.time()), "\n")
cat("=========================================================\n\n")

names(df)


sink()


# ── Subset definitions ────────────────────────────────────────────────────────

temporal    <- c("year", "date", "harvest_year")

spatial     <- c("long", "lat")

plotdesign  <- c("id", "block", "treatment", "data_id", "site", "plot",        # ← fixed split
                 "db_site_id", "db_site_name", "sample_name_db", "sample_name_field",
                 "sample_ordering", "row", "transect", "objectid",
                 "landuse", "signal_code", "land_use")

environmental <- c("aspect", "crop", "orientation", "soil_type", "crop_type",
                   "direction", "harvested_area_m2", "tree_age_years", "distance_to_tree")

yield <- c("yield_wweight", "dm", "dm_content", "X1000seed",
           "crude_fat", "crude_protein", "crude_starch", "dm_straw",
           "biomass_kg_m2", "grain_kg_m2", "total_kg_m2",
           "grass_dm", "grass_crude_protein", "grass_crude_fibre",
           "grass_dm_content", "grass_crude_fat", "grass_sugar",
           "grass_elos", "grass_eulos", "prod",
           "grain_or_corn_standardized", "grain_or_corn_dry_mass",
           "straw_2016_winter_barley", "corn_2016_winter_barley",
           "straw_2017_rapeseed", "corn_2017_rapeseed",
           "product_crop_grass_row_and_mono",
           "straw_or_grass_dry_mass_2016", "grain_corn_or_grass_dry_mass_16",
           "fresh_weight_straw_kg", "fresh_weight_corn_kg",
           "corn_fresh_weight_w_container_g", "corn_dry_weight_w_container_g",
           "container_g", "corn_moisture_content", "corn_dry_weight_t_per_ha",
           "straw_fresh_weight_w_container", "straw_dry_weight_w_container_g",
           "container_1_g", "straw_moisture_content", "straw_dry_weight_t_per_ha")

wood <- c("wood_yield_estimated", "wood_yield_harvested", "litter_dm",
          "wood_yield_estimated_dm", "wood_yield_measured_dm",
          "wood_biomass_winter_2015_16", "wood_biomass_winter_2016_17",
          "wood_increment_2016_2017", "leaf_litter_fall_2016",
          "leaf_litter_dry_mass_2016", "woody_biomass_winter_2016_2017",
          "wood_production_2016_dry_mass")

# ── Coverage check: make sure every column is assigned exactly once ───────────

all_subsets <- list(
  temporal      = temporal,
  spatial       = spatial,
  plotdesign    = plotdesign,
  environmental = environmental,
  yield         = yield,
  wood          = wood
)

all_assigned  <- unlist(all_subsets)
all_cols      <- names(df)


print(setdiff(all_cols, all_assigned))        # should be character(0)

print(setdiff(all_assigned, all_cols))        # should be character(0)

print(all_assigned[duplicated(all_assigned)]) # should be character(0)


# ── Per-subset overview: n valid, n NA, % complete ───────────────────────────

subset_overview <- function(data, cols, subset_name) {
  cols_present <- intersect(cols, names(data))   # guard against missing cols
  
  out <- data.frame(
    subset   = subset_name,
    column   = cols_present,
    n_valid  = sapply(cols_present, \(c) sum(!is.na(data[[c]]))),
    n_na     = sapply(cols_present, \(c) sum( is.na(data[[c]]))),
    pct_complete = sapply(cols_present,
                          \(c) round(mean(!is.na(data[[c]])) * 100, 1)),
    row.names = NULL
  )
  out
}

overview_list <- mapply(subset_overview,
                        cols        = all_subsets,
                        subset_name = names(all_subsets),
                        MoreArgs    = list(data = df),
                        SIMPLIFY    = FALSE)

overview_all <- do.call(rbind, overview_list)

print(overview_all, row.names = FALSE)

# Blcok: Plotdesign  ------------------------------------------------------



levels(as.factor(df$block))

pd <- df[, intersect(plotdesign, names(df))]   # safe subset

# 1. Cardinality of every ID-like column — tells you the hierarchy at a glance
cat("=== Unique values per plotdesign column ===\n")
sort(sapply(pd, \(x) length(unique(x[!is.na(x)]))))

# 2. how many datasets 
levels(as.factor(df$data_id)) #-> 15 datasets 

# 3. how many observations per dataset 
df %>% 
  group_by(data_id)
  dplyr::summarise(count = dplyr::count(row))

# 1. how many lines 

# 1. Is the combination (site × block × plot × treatment) the unique row key?
df %>% 
  count(data_id)

# 2. Is the combination (site × block × plot × treatment) the unique row key?
df |>
  dplyr::count(data_id, block, plot, treatment) |>
  dplyr::filter(n > 1)          # empty → that combo is unique per row

# 3. How many plots per site, blocks per site?
df |> dplyr::summarise(
  n_plots  = dplyr::n_distinct(plot),
  n_blocks = dplyr::n_distinct(block),
  .by = data_id
)

# 4. Are db_site_id / db_site_name / site all synonyms for the same thing?
df |>
  dplyr::distinct(site, db_site_id, db_site_name) |>
  dplyr::arrange(site)

# 5. Treatments per site — catches imbalanced designs
df |>
  dplyr::distinct(site, treatment) |>
  dplyr::count(site, name = "n_treatments")

# 6. Row/transect/objectid — do these nest inside plot?
df |>
  dplyr::distinct(site, plot, row, transect) |>
  dplyr::count(site, plot, name = "n_subunits") |>
  dplyr::arrange(desc(n_subunits))

# 7. sample_name_db vs sample_name_field — are they consistent?
df |>
  dplyr::filter(sample_name_db != sample_name_field) |>
  dplyr::select(site, plot, sample_name_db, sample_name_field) |>
  head(20)

# 8. Full cross-tab: treatments × sites as a matrix
table(df$site, df$treatment)

# treatments and blocks are only in the koch



####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################



library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# ── Define yield columns (from your earlier subset) ──────────────────────────

yield_cols <- c(
  "yield_wweight", "dm", "dm_content", "X1000seed",
  "crude_fat", "crude_protein", "crude_starch", "dm_straw",
  "biomass_kg_m2", "grain_kg_m2", "total_kg_m2",
  "grass_dm", "grass_crude_protein", "grass_crude_fibre",
  "grass_dm_content", "grass_crude_fat", "grass_sugar",
  "grass_elos", "grass_eulos", "prod",
  "grain_or_corn_standardized", "grain_or_corn_dry_mass",
  "straw_2016_winter_barley", "corn_2016_winter_barley",
  "straw_2017_rapeseed", "corn_2017_rapeseed",
  "product_crop_grass_row_and_mono",
  "straw_or_grass_dry_mass_2016", "grain_corn_or_grass_dry_mass_16",
  "fresh_weight_straw_kg", "fresh_weight_corn_kg",
  "corn_fresh_weight_w_container_g", "corn_dry_weight_w_container_g",
  "container_g", "corn_moisture_content", "corn_dry_weight_t_per_ha",
  "straw_fresh_weight_w_container", "straw_dry_weight_w_container_g",
  "container_1_g", "straw_moisture_content", "straw_dry_weight_t_per_ha"
)


# ── 1. Observations per data_id ───────────────────────────────────────────────

obs_per_dataid <- df %>%
  count(data_id, name = "n_obs") %>%
  arrange(desc(n_obs))

print(obs_per_dataid)

obs_per_dataid %>%
  arrange(n_obs) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(data_id=factor(data_id, levels=data_id)) %>%   # This trick update the factor levels
  ggplot(aes(x=data_id, y=n_obs)) + 
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

obs_per_dataid %>%
  filter(data_id != "koch25") %>% 
  arrange(n_obs) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(data_id=factor(data_id, levels=data_id)) %>%   # This trick update the factor levels
  ggplot(aes(x=data_id, y=n_obs)) + 
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 15))

# ── 2. Assign field column ────────────────────────────────────────────────────

df <- df %>%
  mutate(field = case_when(
    str_detect(tolower(data_id), "dornburg")      ~ "Dornburg",
    str_detect(tolower(data_id), "forst")         ~ "Forst",
    str_detect(tolower(data_id), "gladbacherhof") ~ "Gladbacherhof",
    str_detect(tolower(data_id), "koch")          ~ "IhingerHof",
    str_detect(tolower(data_id), "mariensee")     ~ "Mariensee",
    str_detect(tolower(data_id), "wendhausen")    ~ "Wendhausen",
    str_detect(tolower(data_id), "reiffenhausen") ~ "Reiffenhausen",
    # signal16: derive from site column
    data_id == "signal16" & str_detect(tolower(site), "mariensee") ~ "Mariensee",
    data_id == "signal16" ~ site,   # keeps "Dornburg", "Forst", "Wendhausen" as-is
    TRUE ~ NA_character_            # catch-all — review anything landing here
  ))

# Quick check: anything unassigned?
df %>%
  filter(is.na(field)) %>%
  count(data_id, site)


# ── 2.1a. Observations per field ─────────────────────────────────────────────

obs_per_field <- df %>%
  count(field, name = "n_obs") %>%
  arrange(desc(n_obs))

print(obs_per_field)


obs_per_field %>%
  arrange(n_obs) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(field=factor(field, levels=field)) %>%   # This trick update the factor levels
  ggplot(aes(x=field, y=n_obs)) + 
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

obs_per_field %>%
  filter(field != "IhingerHof") %>% 
  arrange(n_obs) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(field=factor(field, levels=field)) %>%   # This trick update the factor levels
  ggplot(aes(x=field, y=n_obs)) + 
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 15))


# ── 2.1b. Observations per year per field ────────────────────────────────────

obs_year_field <- df %>%
  count(field, year, name = "n_obs") %>%
  arrange(field, year)

print(obs_year_field)

obs_year_field %>%
  ggplot(aes(x=as.factor(year), y=n_obs, fill = field)) + 
  geom_bar(stat="identity", alpha=.6, width=.4, position = position_dodge()) +
  xlab("") +
  theme_bw()


obs_year_field %>%
  filter(field != "IhingerHof") %>% 
  ggplot(aes(x=as.factor(year), y=n_obs, fill = field)) + 
  geom_bar(stat="identity", alpha=.6, width=.4, position = position_dodge()) +
  xlab("") +
  theme_bw()

# Wide version — easier to scan gaps
obs_year_field_wide <- obs_year_field %>%
  pivot_wider(names_from = year, values_from = n_obs, values_fill = 0)

print(obs_year_field_wide)



# ── 3. Factor levels of key design variables ─────────────────────────────────

cat("\n=== 3.1 treatment ===\n")
print(sort(unique(df$treatment)))

cat("\n=== 3.2 aspect ===\n")
print(sort(unique(df$aspect)))

cat("\n=== 3.3 crop ===\n")
print(sort(unique(df$crop)))

cat("\n=== 3.4 distance_to_tree ===\n")
print(sort(unique(df$distance_to_tree)))
hist(as.numeric(df$distance_to_tree))

# ── 4. Where do rows without yield_wweight come from? ────────────────────────

# Flag rows where yield_wweight is missing
df_no_wweight <- df %>%
  filter(is.na(yield_wweight))

cat("\n=== Rows without yield_wweight: n =", nrow(df_no_wweight), "===\n")

# 4a. Which data_id do they come from?
cat("\n--- by data_id ---\n")
df_no_wweight %>%
  count(data_id, name = "n_missing") %>%
  arrange(desc(n_missing)) %>%
  print()

# 4b. Which yield variables ARE reported for these rows?
cat("\n--- yield variables reported (n non-NA) for rows missing yield_wweight ---\n")

yield_cols_present <- intersect(yield_cols, names(df))  # guard against any missing

df_no_wweight %>%
  select(all_of(yield_cols_present)) %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "yield_variable", values_to = "n_reported") %>%
  filter(n_reported > 0) %>%
  arrange(desc(n_reported)) %>%
  print(n = Inf)

# 4c. Cross-tab: data_id × which yield variable is filled — for a cleaner picture
cat("\n--- data_id × reported yield variable (n non-NA) ---\n")

df_no_wweight %>%
  select(data_id, all_of(yield_cols_present)) %>%
  group_by(data_id) %>%
  summarise(across(all_of(yield_cols_present), ~ sum(!is.na(.))),
            .groups = "drop") %>%
  pivot_longer(-data_id, names_to = "yield_variable", values_to = "n_reported") %>%
  filter(n_reported > 0) %>%
  arrange(data_id, desc(n_reported)) %>%
  print(n = Inf)
