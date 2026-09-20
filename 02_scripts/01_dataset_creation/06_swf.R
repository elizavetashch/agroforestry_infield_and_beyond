# =============================================================================
# Step 6 — Semi-natural woody feature (SWF) extraction and merge
# Input:  output/swf/<year>/*_buf3000m*.tif  (binary woody-feature rasters)
#         data/ArcGIS_Outputs/fieldpolygons.csv
#         data/AnalysisData/AF_slope.csv
# Output: output/swf/swf_annulus_by_distance.csv
#         output/swf/swf_within_field.csv
#         data/AnalysisData/AF_swf.csv   ← final analysis-ready dataset
# =============================================================================
# SWF proportions are extracted for three reference years:
#   2015 → applied to yield year 2016
#   2018 → applied to yield years 2017-2019
#   2021 → applied to yield years 2020+
#
# Two spatial summaries per site per year:
#   (a) Annulus profile — 100 m steps from the field boundary out to 1000 m
#   (b) Field interior  — proportion within the field boundary disk itself
# =============================================================================

library(sf)
library(terra)        # only terra — raster package not needed
library(dplyr)
library(tidyr)
library(readr)
library(geosphere)

ANALYSIS_DIR <- "C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond\\01_Data\\AnalysisData"
SWF_OUT <- "C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond\\01_Data\\AnalysisData\\SWF"
DISTANCES    <- seq(100, 1000, by = 100)

field_ids <- c("Ihinger", "Mariensee", "Gladbacherhof", "Forst",
               "Dornburg", "Wendhausen", "Vechta", "Reiffenhausen")

# =============================================================================
# (1) Load field polygons and compute boundary radius
# =============================================================================

fieldpolygons <- read.csv("01_Data/AnalysisData/fieldpolygons.csv") %>%
  mutate(
    fieldlength  = distHaversine(
      cbind(minLongitude, minLatitude),
      cbind(maxLongitude, maxLatitude)
    ),
    boundary_min = fieldlength / 2
  )

fields_sf <- st_as_sf(
  fieldpolygons,
  coords = c("Longitude", "Latitude"),
  crs    = 4326
) %>%
  st_transform(3035)

# =============================================================================
# (2) Extraction functions
# =============================================================================

# Annulus profile: concentric rings starting at field boundary
extract_annulus_profile <- function(point_coords, raster, boundary_min,
                                    distances = DISTANCES) {
  lapply(seq_along(distances), function(idx) {
    r_outer <- boundary_min + distances[idx]
    r_inner <- if (idx == 1) boundary_min else boundary_min + distances[idx - 1]

    annulus_mask <- st_difference(
      st_buffer(point_coords, dist = r_outer),
      st_buffer(point_coords, dist = r_inner)
    )
    vals <- terra::extract(raster, vect(annulus_mask))[[2]]
    vals <- vals[!is.na(vals)]
    prop <- if (length(vals) > 0) sum(vals) / length(vals) else NA_real_

    data.frame(distance = distances[idx], radius = r_outer, prop_swf = prop, n_swf = sum(vals))
  }) |> bind_rows()
}

# Field interior: single disk of radius = boundary_min
extract_field_interior <- function(point_coords, raster, boundary_min) {
  vals <- terra::extract(raster, vect(st_buffer(point_coords, dist = boundary_min)))[[2]]
  vals <- vals[!is.na(vals)]
  prop <- if (length(vals) > 0) sum(vals) / length(vals) else NA_real_
  data.frame(radius = boundary_min, prop_swf = prop, n_swf = sum(vals))
}

# Process one reference year's raster directory
process_year <- function(raster_dir, pattern, fields_sf, field_ids) {
  raster_files <- list.files(raster_dir, pattern = pattern, full.names = TRUE)
  # Match files to field IDs via filename prefix
  file_ids <- gsub("_.*", "", basename(raster_files))

  fields_sf$id <- field_ids

  annulus_list  <- list()
  interior_list <- list()

  for (i in seq_along(raster_files)) {
    fid       <- file_ids[i]
    field_row <- fields_sf[fields_sf$id == fid, ]
    if (nrow(field_row) == 0) {
      warning("No field match for: ", fid)
      next
    }

    woody_binary <- as.numeric(rast(raster_files[i]))
    bmin         <- field_row$boundary_min[1]
    coords       <- st_geometry(field_row)

    ann  <- extract_annulus_profile(coords, woody_binary, bmin)
    ann$id <- fid
    annulus_list[[fid]] <- ann

    int  <- extract_field_interior(coords, woody_binary, bmin)
    int$id <- fid
    interior_list[[fid]] <- int

    message("  Processed: ", fid)
  }

  list(
    annulus  = bind_rows(annulus_list),
    interior = bind_rows(interior_list)
  )
}

# =============================================================================
# (3) Run extraction for each reference year
# =============================================================================

if (RUN_SWF_RASTERS) {
  message("Extracting SWF — 2015...")
  res2015 <- process_year(file.path(SWF_OUT, "2015"), "_buf3000m_binary\\.tif$",
                          fields_sf, field_ids)
  message("Extracting SWF — 2018...")
  res2018 <- process_year(file.path(SWF_OUT, "2018"), "_buf3000m\\.tif$",
                          fields_sf, field_ids)
  message("Extracting SWF — 2021...")
  res2021 <- process_year(file.path(SWF_OUT, "2021"), "_buf3000m\\.tif$",
                          fields_sf, field_ids)

  # Annulus profiles (wide: one prop_swf column per reference year)
  annulus_all <- res2015$annulus %>% rename(prop_swf.2015 = prop_swf, n_swf.2015 = n_swf) %>%
    full_join(res2018$annulus %>% rename(prop_swf.2018 = prop_swf, n_swf.2018 = n_swf),
              by = c("id", "distance")) %>%
    full_join(res2021$annulus %>% rename(prop_swf.2021 = prop_swf, n_swf.2021 = n_swf),
              by = c("id", "distance")) %>%
    select(id, distance, starts_with("radius"), starts_with("prop_swf"), starts_with("n_swf")) %>%
    arrange(id, distance)
  
  # Standardise IDs
  fix_id <- function(x) gsub("Ihinger$", "IhingerHof", x)
  annulus_all$id  <- fix_id(annulus_all$id)
  write_csv(annulus_all, file.path(SWF_OUT, "swf_annulus_by_distance.csv"))

  # Field interior proportions
  interior_all <- res2015$interior %>% select(id, prop_swf.2015 = prop_swf) %>%
    full_join(res2018$interior %>% select(id, prop_swf.2018 = prop_swf), by = "id") %>%
    full_join(res2021$interior %>% select(id, prop_swf.2021 = prop_swf), by = "id") %>%
    arrange(id)
  
  interior_all$id <- fix_id(interior_all$id)
  write_csv(interior_all, file.path(SWF_OUT, "swf_within_field.csv"))
  
  message("Saved SWF CSVs.")
}

# =============================================================================
# (4) Merge SWF with the main dataset
# =============================================================================

if (RUN_SWF_MERGE) {
  
  df           <- read.csv(file.path(ANALYSIS_DIR, "AF_slope.csv"))
  annulus_all  <- read_csv(file.path(SWF_OUT, "swf_annulus_by_distance.csv"))
  interior_all <- read_csv(file.path(SWF_OUT, "swf_within_field.csv"))

  # If re-running: drop old SWF columns to avoid duplicates
  swf_cols <- c("prop_swf", "swf_year", "radius", "prop_swf_within")
  df <- df %>% select(-any_of(swf_cols))

  # Standardise IDs
  fix_id <- function(x) gsub("Ihinger$", "IhingerHof", x)
  annulus_all$id  <- fix_id(annulus_all$id)
  interior_all$id <- fix_id(interior_all$id)

  # Map each yield year to the appropriate SWF reference year
  df <- df %>%
    mutate(swf_year = case_when(
      year == 2016            ~ "prop_swf.2015",
      year %in% 2017:2019    ~ "prop_swf.2018",
      year %in% 2020:2024            ~ "prop_swf.2021"
    ))

  swf_long <- annulus_all %>%
    pivot_longer(starts_with("prop_swf."),
                 names_to  = "swf_year",
                 values_to = "prop_swf")

  int_long <- interior_all %>%
    pivot_longer(starts_with("prop_swf."),
                 names_to  = "swf_year",
                 values_to = "prop_swf_within")

  dfswf <- df %>%
    left_join(swf_long, by = c("field" = "id", "swf_year"),
              relationship = "many-to-many") %>%
    left_join(int_long, by = c("field" = "id", "swf_year"),
              relationship = "many-to-many")
  #write_csv(dfswf, "01_Data/AF_swf.csv")
  write_csv(dfswf, file.path(ANALYSIS_DIR, "AF_swf.csv"))
  message("\nStep 6 complete — AF_swf.csv  (", nrow(dfswf), " rows)  ← FINAL DATASET")
}
