# =============================================================================
# Step 3 — Download DWD grid climate data and extract growing-season summaries
# Input:  data/AnalysisData/AF_merged.csv
# Output: data/climate/rdwd/climate_precip.csv
#         data/climate/rdwd/climate_temp.csv
#         data/climate/rdwd/climate_sun.csv
#         data/AnalysisData/AF_climate.csv
# =============================================================================
# Variables extracted per observation (growing season window):
#   temp_C_mean    — mean monthly temperature (°C)
#   sun_MJ_m2_mean — mean monthly global radiation (MJ/m²)
#   precip_mm_sum  — total precipitation (mm)
#   fieldlength    — Haversine diagonal of the field bounding box (m)
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(terra)
library(rdwd)
library(geosphere)

ANALYSIS_DIR <- "C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond\\01_Data\\AnalysisData"
CLIMATE_DIR   <- "01_Data\\AnalysisData\\climate/rdwd"
dir.create(CLIMATE_DIR, recursive = TRUE, showWarnings = FALSE)

YEARS <- 2015:2023   # growing seasons span back to Oct 2015

# =============================================================================
# (1) Load base dataset
# =============================================================================

df <- read_csv(file.path(ANALYSIS_DIR, "AF_merged.csv")) %>%
  mutate(
    gs_start_date = as.Date(gs_start_date),
    gs_end_date   = as.Date(gs_end_date)
  )

fields <- df %>%
  select(field, latitude = latitude, longitude = longitude) %>%
  distinct()

loc <- data.frame(x = fields$longitude, y = fields$latitude)

# =============================================================================
# (2) Download DWD monthly grids
# =============================================================================

rdwd::updateRdwd()
data("gridIndex")

# Filter to the relevant years
year_pattern <- paste(YEARS, collapse = "|")

precip_paths <- grep("monthly/precipitation",      gridIndex, value = TRUE)
temp_paths   <- grep("monthly/air_temperature_mean", gridIndex, value = TRUE)
sun_paths    <- grep("monthly/sun",                gridIndex, value = TRUE)

precip_paths <- grep(year_pattern, precip_paths, value = TRUE)
temp_paths   <- grep(year_pattern, temp_paths,   value = TRUE)
sun_paths    <- grep(year_pattern, sun_paths,    value = TRUE)

rprecip <- dataDWD(precip_paths, base = gridbase, joinbf = TRUE)
rtemp   <- dataDWD(temp_paths,   base = gridbase, joinbf = TRUE)
rsun    <- dataDWD(sun_paths,    base = gridbase, joinbf = TRUE)

# =============================================================================
# (3) Helper: extract monthly grid → long table
# =============================================================================

# Column names follow: YYYY-MM-01
make_date_names <- function(years, n_months_per_year = 12) {
  format(
    as.Date(paste0(
      rep(years, times = n_months_per_year),
      "-",
      rep(sprintf("%02d", 1:n_months_per_year), each = length(years)),
      "-01"
    )),
    "%Y-%m-%d"
  )
}

extract_grid_long <- function(raster_list, loc, field_names, value_col) {
  stack  <- terra::rast(raster_list)
  stack  <- projectRasterDWD(stack, proj = "seasonal", extent = "seasonal")
  wide   <- terra::extract(stack, loc)[, -1]  # drop ID column
  colnames(wide) <- make_date_names(YEARS)
  wide$field <- field_names

  wide %>%
    pivot_longer(-field, names_to = "date", values_to = value_col) %>%
    mutate(date = as.Date(date),
           year  = format(date, "%Y"),
           month = format(date, "%m"))
}

message("Extracting precipitation...")
preciptable <- extract_grid_long(rprecip, loc, fields$field, "prec_mm")
write_csv(preciptable, file.path(CLIMATE_DIR, "climate_precip.csv"))

message("Extracting temperature...")
temptable <- extract_grid_long(rtemp, loc, fields$field, "temp_C")
write_csv(temptable, file.path(CLIMATE_DIR, "climate_temp.csv"))

message("Extracting sunshine duration...")
suntable <- extract_grid_long(rsun, loc, fields$field, "sun_MJ_m2")
write_csv(suntable, file.path(CLIMATE_DIR, "climate_sun.csv"))

# =============================================================================
# (4) Aggregate to growing-season summaries per observation
# =============================================================================

message("Aggregating climate to growing-season windows...")

result <- df %>%
  rowwise() %>%
  mutate(
    temp_C_mean = mean(
      temptable$temp_C[
        temptable$field == field &
          temptable$date >= gs_start_date &
          temptable$date <= gs_end_date
      ], na.rm = TRUE
    ),
    sun_MJ_m2_mean = mean(
      suntable$sun_MJ_m2[
        suntable$field == field &
          suntable$date >= gs_start_date &
          suntable$date <= gs_end_date
      ], na.rm = TRUE
    ),
    precip_mm_sum = sum(
      preciptable$prec_mm[
        preciptable$field == field &
          preciptable$date >= gs_start_date &
          preciptable$date <= gs_end_date
      ], na.rm = TRUE
    )
  ) %>%
  ungroup()

# =============================================================================
# (5) Add field diagonal length (Haversine) from bounding box corners
# =============================================================================

result <- result %>%
  mutate(
    fieldlength = distHaversine(
      cbind(min_longitude, min_latitude),
      cbind(max_longitude, max_latitude)
    )
  )

write_csv(result, file.path(ANALYSIS_DIR, "AF_climate.csv"))
message("\nStep 3 complete — AF_climate.csv  (", nrow(result), " rows)")
