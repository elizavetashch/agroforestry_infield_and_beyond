
# =============================================================================
# Dataset Structure and Design Exploration
# Created: 2026-06-15
# Datasets: koch25, wendhausen, gladbacherhof, bremsberg,
#            mariensee, dornburg, reiffenhausen
#
# NOTE: Run 20260614_full_exploration.R first to load all datasets,
#       OR source() it here:
# source("scripts/20260614_full_exploration.R")
# =============================================================================


# Packages --------------------------------------------------------------------

library(tidyverse)
library(ggplot2)


# Create output folder --------------------------------------------------------

dir.create("output", showWarnings = FALSE)


# =============================================================================
# 1. RECORD DIM() AND STR() TO TXT FILE
# =============================================================================

datasets <- list(
  koch25        = koch25,
  wendhausen    = wendhausen,
  gladbacherhof = gladbacherhof,
  bremsberg     = bremsberg,
  mariensee     = mariensee,
  dornburg      = dornburg,
  reiffenhausen = reiffenhausen
)

sink("output/dataset_structures.txt")

cat("=========================================================\n")
cat("DATASET STRUCTURES - Generated:", format(Sys.time()), "\n")
cat("=========================================================\n\n")

for (nm in names(datasets)) {
  cat("\n---------------------------------------------------------\n")
  cat("DATASET:", nm, "\n")
  cat("---------------------------------------------------------\n")
  cat("dim (rows x cols):", dim(datasets[[nm]]), "\n\n")
  cat("str:\n")
  str(datasets[[nm]])
  cat("\n")
}

sink()
cat("Saved: output/dataset_structures.txt\n")


# =============================================================================
# 2. YIELD DISTRIBUTIONS - HISTOGRAMS AND SCATTERPLOTS
# =============================================================================
# If you are unsure which variable is yield, all candidates are plotted.


# Koch25 ----------------------------------------------------------------------
# yield_wweight = wet weight yield

hist(koch25$yield_wweight,
     main = "Koch25: yield_wweight", xlab = "yield_wweight")

ggplot(koch25, aes(p_dist, yield_wweight, color = crop)) +
  geom_point(alpha = 0.7) +
  labs(title = "Koch25: Yield vs. Distance to Tree", x = "p_dist", y = "yield_wweight") +
  theme_bw()


# Wendhausen ------------------------------------------------------------------
# dm = crop dry matter yield; wood_yield_estimated/harvested = tree yield

hist(wendhausen$dm,
     main = "Wendhausen: dm (crop dry matter)", xlab = "dm")


# Outlier Wendhausen ------------------------------------------------------
wendhausen <- 
wendhausen %>% 
  filter(dm < 3.22821629407e14) 

# Weiter ------------------------------------------------------------------

hist(wendhausen$wood_yield_estimated,
     main = "Wendhausen: wood_yield_estimated", xlab = "wood_yield_estimated")

ggplot(wendhausen, aes(distance_to_tree_strip, dm, color = crop)) +
  geom_point(alpha = 0.7) +
  labs(title = "Wendhausen: Crop DM vs. Distance to Tree",
       x = "distance_to_tree_strip", y = "dm") +
  theme_bw()

ggplot(wendhausen, aes(distance_to_tree_strip, wood_yield_harvested, color = crop)) +
  geom_point(alpha = 0.7) +
  labs(title = "Wendhausen: Wood Yield (harvested) vs. Distance to Tree",
       x = "distance_to_tree_strip", y = "wood_yield_harvested") +
  theme_bw()


# Gladbacherhof ---------------------------------------------------------------
# biomass_kg_m2, grain_kg_m2, total_kg_m2 - all three candidate yield columns

hist(gladbacherhof$biomass_kg_m2,
     main = "Gladbacherhof: biomass_kg_m2", xlab = "biomass_kg_m2")

hist(gladbacherhof$grain_kg_m2,
     main = "Gladbacherhof: grain_kg_m2", xlab = "grain_kg_m2")

hist(gladbacherhof$total_kg_m2,
     main = "Gladbacherhof: total_kg_m2", xlab = "total_kg_m2")

ggplot(gladbacherhof, aes(distance, biomass_kg_m2, color = crop)) +
  geom_point(alpha = 0.7) +
  labs(title = "Gladbacherhof: Biomass vs. Distance",
       x = "distance", y = "biomass_kg_m2") +
  theme_bw()

ggplot(gladbacherhof, aes(distance, grain_kg_m2, color = crop)) +
  geom_point(alpha = 0.7) +
  labs(title = "Gladbacherhof: Grain Yield vs. Distance",
       x = "distance", y = "grain_kg_m2") +
  theme_bw()


# Bremsberg -------------------------------------------------------------------
# same columns as gladbacherhof

hist(bremsberg$biomass_kg_m2,
     main = "Bremsberg: biomass_kg_m2", xlab = "biomass_kg_m2")

hist(bremsberg$grain_kg_m2,
     main = "Bremsberg: grain_kg_m2", xlab = "grain_kg_m2")

hist(bremsberg$total_kg_m2,
     main = "Bremsberg: total_kg_m2", xlab = "total_kg_m2")

ggplot(bremsberg, aes(distance, biomass_kg_m2, color = crop)) +
  geom_point(alpha = 0.7) +
  labs(title = "Bremsberg: Biomass vs. Distance",
       x = "distance", y = "biomass_kg_m2") +
  theme_bw()

ggplot(bremsberg, aes(distance, grain_kg_m2, color = crop)) +
  geom_point(alpha = 0.7) +
  labs(title = "Bremsberg: Grain Yield vs. Distance",
       x = "distance", y = "grain_kg_m2") +
  theme_bw()


# Mariensee -------------------------------------------------------------------
# grass_dm = grass dry matter; wood yields as additional candidates

hist(mariensee$grass_dm,
     main = "Mariensee: grass_dm", xlab = "grass_dm")

hist(mariensee$wood_yield_estimated_dm,
     main = "Mariensee: wood_yield_estimated_dm", xlab = "wood_yield_estimated_dm")

hist(mariensee$wood_yield_measured_dm,
     main = "Mariensee: wood_yield_measured_dm", xlab = "wood_yield_measured_dm")

ggplot(mariensee, aes(distance_to_tree_strip, grass_dm)) +
  geom_point(alpha = 0.7) +
  labs(title = "Mariensee: Grass DM vs. Distance to Tree",
       x = "distance_to_tree_strip", y = "grass_dm") +
  theme_bw()


# Dornburg --------------------------------------------------------------------
# prod = production / yield

hist(dornburg$prod,
     main = "Dornburg: prod", xlab = "prod")

ggplot(dornburg, aes(dist, prod)) +
  geom_point(alpha = 0.7) +
  labs(title = "Dornburg: prod vs. dist",
       x = "dist (treatment zone)", y = "prod") +
  theme_bw()


# Reiffenhausen ---------------------------------------------------------------
# Wide format: each crop x year has its own column
# Pivot to long for plotting

reiffenhausen_long <- reiffenhausen |>
  pivot_longer(
    cols = c(straw_2016_winter_barley, corn_2016_winter_barley,
             straw_2017_rapeseed, corn_2017_rapeseed,
             wood_biomass_winter_2015_16, wood_biomass_winter_2016_17),
    names_to  = "measure",
    values_to = "yield"
  )

hist(reiffenhausen_long$yield,
     main = "Reiffenhausen: all yield columns", xlab = "yield")

ggplot(reiffenhausen_long, aes(distance_from_tree_row, yield, color = measure)) +
  geom_point(alpha = 0.7) +
  labs(title = "Reiffenhausen: Yield vs. Distance from Tree Row",
       x = "distance_from_tree_row", y = "yield") +
  theme_bw()


# =============================================================================
# 3. STUDY DESIGN PLOTS
# =============================================================================
# Following the same structure as for Koch25.
# For each dataset:
#   (a) distinct design table
#   (b) count summary
#   (c) treatment visualisation (spatial)
#   (d) crop rotation (where crop column exists)


# Koch25 ----------------------------------------------------------------------

design_koch25 <- koch25 |>
  distinct(year, block, id, treatment, p_dist, crop, lat, long)

koch25 |>
  count(block, id, treatment) |>
  arrange(block, id)

design_koch25 |>
  count(year, block, p_dist, treatment) |>
  tidyr::pivot_wider(
    names_from  = treatment,
    values_from = n,
    values_fill = 0
  )

# TREATMENT VISUALISATION
ggplot(design_koch25,
       aes(long, lat,
           color = treatment,
           size  = p_dist)) +
  geom_point(alpha = 0.8) +
  coord_equal() +
  theme_bw() +
  labs(title = "Koch25: Treatment Design")

# CROP ROTATION
ggplot(design_koch25,
       aes(long, lat,
           color = crop,
           size  = p_dist)) +
  geom_point(alpha = 0.8) +
  facet_wrap(~year) +
  coord_equal() +
  theme_bw() +
  labs(title = "Koch25: Crop Rotation")


# Wendhausen ------------------------------------------------------------------

design_wendhausen <- wendhausen |>
  distinct(year, site, plot, orientation, distance_to_tree_strip, crop, lat, long)

wendhausen |>
  count(site, plot, orientation) |>
  arrange(site, plot)

design_wendhausen |>
  count(year, site, distance_to_tree_strip, orientation) |>
  tidyr::pivot_wider(
    names_from  = orientation,
    values_from = n,
    values_fill = 0
  )

# TREATMENT VISUALISATION
ggplot(design_wendhausen,
       aes(long, lat,
           color = orientation,
           size  = distance_to_tree_strip)) +
  geom_point(alpha = 0.8) +
  coord_equal() +
  theme_bw() +
  labs(title = "Wendhausen: Treatment Design (orientation)")

# CROP ROTATION
ggplot(design_wendhausen,
       aes(long, lat,
           color = crop,
           size  = distance_to_tree_strip)) +
  geom_point(alpha = 0.8) +
  facet_wrap(~year) +
  coord_equal() +
  theme_bw() +
  labs(title = "Wendhausen: Crop Rotation")


# Gladbacherhof ---------------------------------------------------------------

design_gladbacherhof <- gladbacherhof |>
  distinct(year, row, transect, direction, distance, crop, lat, long)

gladbacherhof |>
  count(row, transect, direction) |>
  arrange(row, transect)

design_gladbacherhof |>
  count(year, direction, distance, crop) |>
  tidyr::pivot_wider(
    names_from  = direction,
    values_from = n,
    values_fill = 0
  )

# TREATMENT VISUALISATION
ggplot(design_gladbacherhof,
       aes(long, lat,
           shape = direction,
           color = distance)) +
  geom_point(alpha = 0.8, size = 3) +
  coord_equal() +
  theme_bw() +
  labs(title = "Gladbacherhof: Treatment Design (direction / distance)")

# CROP ROTATION
ggplot(design_gladbacherhof,
       aes(long, lat,
           color = crop)) +
  geom_point(alpha = 0.8, size = 3) +
  facet_wrap(~year) +
  coord_equal() +
  theme_bw() +
  labs(title = "Gladbacherhof: Crop Rotation")


# Bremsberg -------------------------------------------------------------------

design_bremsberg <- bremsberg |>
  distinct(year, row, transect, direction, distance, crop, lat, long)

bremsberg |>
  count(row, transect, direction) |>
  arrange(row, transect)

design_bremsberg |>
  count(year, direction, distance, crop) |>
  tidyr::pivot_wider(
    names_from  = direction,
    values_from = n,
    values_fill = 0
  )

# TREATMENT VISUALISATION
ggplot(design_bremsberg,
       aes(long, lat,
           color = direction,
           shape = distance)) +
  geom_point(alpha = 0.8, size = 3) +
  coord_equal() +
  theme_bw() +
  labs(title = "Bremsberg: Treatment Design (direction / distance)")

# CROP ROTATION
ggplot(design_bremsberg,
       aes(long, lat,
           color = crop)) +
  geom_point(alpha = 0.8, size = 3) +
  facet_wrap(~year) +
  coord_equal() +
  theme_bw() +
  labs(title = "Bremsberg: Crop Rotation")


# Mariensee -------------------------------------------------------------------
# No crop column - only treatment orientation and distance

design_mariensee <- mariensee |>
  distinct(year, site, plot, orientation, distance_to_tree_strip, lat, long)

mariensee |>
  count(site, plot, orientation) |>
  arrange(site, plot)

design_mariensee |>
  count(year, site, distance_to_tree_strip, orientation) |>
  tidyr::pivot_wider(
    names_from  = orientation,
    values_from = n,
    values_fill = 0
  )

# TREATMENT VISUALISATION
ggplot(design_mariensee,
       aes(long, lat,
           color = orientation,
           size  = distance_to_tree_strip)) +
  geom_point(alpha = 0.8) +
  facet_wrap(~year) +
  coord_equal() +
  theme_bw() +
  labs(title = "Mariensee: Treatment Design (orientation)")

# No crop rotation plot (no crop column in mariensee)


# Dornburg --------------------------------------------------------------------
# Single year, no crop column; dist = treatment zone (T = tree row etc.)

design_dornburg <- dornburg |>
  distinct(dist, lat, long)

dornburg |>
  count(dist)

# TREATMENT VISUALISATION
ggplot(design_dornburg,
       aes(long, lat,
           color = dist)) +
  geom_point(alpha = 0.8, size = 3) +
  coord_equal() +
  theme_bw() +
  labs(title = "Dornburg: Treatment Design (dist zone)")

# No crop rotation plot (no year or crop column in dornburg)


# Reiffenhausen ---------------------------------------------------------------
# Wide format; lat/long are a single fixed coordinate pair for the whole site.
# Spatial spread per plot is not available -> design table only

design_reiffenhausen <- reiffenhausen |>
  distinct(distance_from_tree_row, land_use, soil_type)

reiffenhausen |>
  count(distance_from_tree_row, land_use) |>
  arrange(distance_from_tree_row)

# TREATMENT VISUALISATION
# NOTE: all points share the same lat/long (site-level coordinate),
# so the spatial plot shows overlap - use as confirmation only.
ggplot(reiffenhausen,
       aes(long, lat,
           color = distance_from_tree_row,
           shape = land_use)) +
  geom_point(alpha = 0.8, size = 3,
             position = position_jitter(width = 0.001, height = 0.001)) +
  coord_equal() +
  theme_bw() +
  labs(title = "Reiffenhausen: Treatment Design (jittered - single site coordinate)")

# No crop rotation plot (wide format, no year/crop columns)
