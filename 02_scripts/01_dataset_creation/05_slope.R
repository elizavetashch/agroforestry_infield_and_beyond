# =============================================================================
# Step 5 — Add slope data (derived from ArcGIS DTM zonal statistics)
# Input:  data/AnalysisData/AF_lulc.csv
#         data/dtm/20260805_slope_fields.csv
# Output: data/AnalysisData/AF_slope.csv
# =============================================================================
# The slope raster was created in ArcGIS:
#   1. DTM from data/dtm/
#   2. Slope tool → degrees
#   3. Clip to field polygon
#   4. Zonal Statistics as Table → the CSV used here
# =============================================================================

library(dplyr)
library(readr)

ANALYSIS_DIR <- "C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond\\01_Data\\AnalysisData"

field_name_map <- c(
  "Ihinger Hof Field"   = "IhingerHof",
  "Dornburg field"      = "Dornburg",
  "Forst Field"         = "Forst",
  "Gladbacherhof Field" = "Gladbacherhof",
  "Mariensee Field"     = "Mariensee",
  "Reiffenhausen Field" = "Reiffenhausen",
  "Vechta Field"        = "Vechta",
  "Wendhausen field"    = "Wendhausen"
)

df    <- read.csv(file.path(ANALYSIS_DIR, "AF_lulc.csv"))
setwd("C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RProject")
slope <- read.csv("data/dtm/20260805_slope_fields.csv")

slope_clean <- slope |>
  mutate(
    field      = recode(Name, !!!field_name_map),
    min_slope  = MIN,
    max_slope  = MAX,
    mean_slope = MEAN
  ) |>
  select(field, min_slope, max_slope, mean_slope)

dfslope <- df %>%
  left_join(slope_clean, by = "field", relationship = "many-to-one")

write_csv(dfslope, file.path(ANALYSIS_DIR, "AF_slope.csv"))
message("Step 5 complete — AF_slope.csv  (", nrow(dfslope), " rows)")
