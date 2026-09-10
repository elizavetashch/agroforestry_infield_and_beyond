# =============================================================================
# Step 7 — Extract soil texture from SoilGrids TIF files
# Input:  01_Data/AnalysisData/SoilGrids/*.tif
#         data/ArcGIS_Outputs/fieldpolygons.csv
# Output: 01_Data/AnalysisData/SoilGrids/soiltexture.csv
# =============================================================================
# SoilGrids layers are extracted at field centroids using simple sampling.
# This produces one row per field with all available SoilGrids properties.
# NOTE: this output is not yet merged into the main pipeline; join it to
#       AF_swf.csv using the "field" column when needed.
# =============================================================================

library(terra)
library(sf)
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

# Load all SoilGrids TIFs as a multi-layer raster
tif_files <- list.files("01_Data/AnalysisData/SoilGrids", pattern = "\\.tif$", full.names = TRUE)
if (length(tif_files) == 0) stop("No TIF files found in 01_Data/AnalysisData/SoilGrids/")

r <- terra::rast(tif_files)
message("SoilGrids layers: ", paste(names(r), collapse = ", "))
message("CRS: ", crs(r, describe = TRUE)$name)

# Load field centroids and reproject to match the raster CRS
fieldpolygons <- read.csv("01_Data/AnalysisData/fieldpolygons.csv")

fieldpolygons_proj <- fieldpolygons %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(r))

coords <- st_coordinates(fieldpolygons_proj)

# Extract raster values at field centroids
values <- terra::extract(r, coords, method = "simple")

# Combine with field names
result <- bind_cols(
  data.frame(
    field_raw = fieldpolygons$Name,
    field     = dplyr::recode(fieldpolygons$Name, !!!field_name_map)
  ),
  values
) 

write_csv(result, "01_Data/AnalysisData/SoilGrids/soiltexture.csv")

message("Step 7 complete — 01_Data/AnalysisData/SoilGrids/soiltexture.csv  (", nrow(result), " rows)")
