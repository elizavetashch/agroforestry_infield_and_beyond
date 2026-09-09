# =============================================================================
# Step 4 — Land-use / land-cover (LULC) processing
# Input:  data/LULC/<year>/*.tif  (ESA WorldCover or equivalent)
#         data/ArcGIS_Outputs/fieldpolygons.csv
#         data/AnalysisData/AF_climate.csv
# Output: output/LULC/*_buf3000m.tif   (clipped rasters — written once)
#         data/AnalysisData/AF_lulc_metrics.csv
#         data/AnalysisData/AF_lulc.csv
# =============================================================================
# Landscape metrics (all at landscape level, 3 km buffer):
#   l_contag — contagion (spatial configuration)
#   l_np     — number of patches (fragmentation)
#   l_sidi   — Simpson diversity index (composition)
#   l_shdi   — Shannon diversity index (composition)
#   l_ai     — aggregation index
#   l_ed     — edge density (configuration)
# =============================================================================

library(terra)
library(sf)
library(dplyr)
library(readr)
library(landscapemetrics)   # lsm_l_* functions

ANALYSIS_DIR <- "C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond\\01_Data\\AnalysisData"
LULC_OUT     <- "01_Data\\AnalysisData\\LULC"
dir.create(LULC_OUT, recursive = TRUE, showWarnings = FALSE)

YEARS      <- 2017:2023
BUFFER_M   <- 3000

lulc_legend <- data.frame(
  value = c(1, 2, 4, 5, 7, 8, 9, 10, 11),
  class = c("Water", "Trees", "Flooded Vegetation", "Crops",
            "Built Area", "Bare Ground", "Snow/Ice", "Clouds", "Rangeland")
)

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

# Helper: clean filename-safe site name
safe_name <- function(x) gsub("[^a-zA-Z0-9]", "_", x)

# =============================================================================
# (1 & 2) Clip rasters to 3 km buffers — all sites
#   Forst sits in UTM zone 33U; all others in 32U.
#   Process each UTM zone separately using its own raster set.
# =============================================================================

setwd("C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RProject")

if (RUN_LULC_RASTERS) {

  clip_rasters_for_zone <- function(utm_zone, fields_sf_filtered) {

    rasters <- lapply(YEARS, function(yr) {
      f <- list.files(
        path    = file.path("data/LULC", yr),
        pattern = paste0("^", utm_zone, "_"),
        full.names = TRUE
      )
      if (length(f) == 0) stop("No raster found for zone ", utm_zone, " year ", yr)
      terra::rast(f[1])
    })

    for (i in seq_len(nrow(fields_sf_filtered))) {
      for (j in seq_along(YEARS)) {
        yr        <- YEARS[j]
        site_name <- fields_sf_filtered$Name[i]
        poly      <- vect(fields_sf_filtered[i, ])

        levels(rasters[[j]]) <- lulc_legend
        r_crop  <- crop(rasters[[j]], poly)
        r_mask  <- mask(r_crop, poly)

        fname <- file.path(LULC_OUT,
                           paste0(safe_name(site_name), "_", yr, "_buf", BUFFER_M, "m.tif"))
        writeRaster(r_mask, fname, overwrite = TRUE)
        message("  Saved: ", fname)
      }
    }
  }

  fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")

  # Use zone 32U raster for reference CRS when projecting points
  ref_raster_32 <- terra::rast(
    list.files("data/LULC/2017", pattern = "^32U_", full.names = TRUE)[1]
  )
  ref_raster_33 <- terra::rast(
    list.files("data/LULC/2017", pattern = "^33U_", full.names = TRUE)[1]
  )

  fields_sf_32 <- fieldpolygons %>%
    filter(Name != "Forst Field") %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(crs(ref_raster_32)) %>%
    st_buffer(dist = BUFFER_M)

  fields_sf_33 <- fieldpolygons %>%
    filter(Name == "Forst Field") %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(crs(ref_raster_33)) %>%
    st_buffer(dist = BUFFER_M)

  message("Clipping 32U rasters (all sites except Forst)...")
  clip_rasters_for_zone("32U", fields_sf_32)

  message("Clipping 33U rasters (Forst)...")
  clip_rasters_for_zone("33U", fields_sf_33)

}

# =============================================================================
# (3) Landscape metrics
# =============================================================================

if (RUN_LULC_METRICS) {

  fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")

  results <- lapply(fieldpolygons$Name, function(site_name) {
    lapply(YEARS, function(yr) {
      fname <- file.path(LULC_OUT,
                         paste0(safe_name(site_name), "_", yr, "_buf", BUFFER_M, "m.tif"))
      if (!file.exists(fname)) {
        warning("Raster not found, skipping: ", fname)
        return(NULL)
      }
      r <- terra::rast(fname)
      levels(r) <- lulc_legend
      message("  Metrics: ", site_name, " ", yr)

      data.frame(
        field    = site_name,
        year     = yr,
        l_contag = lsm_l_contag(r) |> pull(value),
        l_np     = lsm_l_np(r)     |> pull(value),
        l_sidi   = lsm_l_sidi(r)   |> pull(value),
        l_shdi   = lsm_l_shdi(r)   |> pull(value),
        l_ai     = lsm_l_ai(r)     |> pull(value),
        l_ed     = lsm_l_ed(r)     |> pull(value)
      )
    }) |> bind_rows()
  }) |> bind_rows()

  # 2016 has no satellite coverage — carry forward 2017 values
  results_2016 <- results |>
    filter(year == 2017) |>
    mutate(year = 2016)

  results <- bind_rows(results, results_2016) |>
    arrange(field, year)

  write_csv(results, file.path(ANALYSIS_DIR, "AF_lulc_metrics.csv"))
  message("Saved: AF_lulc_metrics.csv")
}

# =============================================================================
# (4) Merge landscape metrics with the main dataset
# =============================================================================

if (RUN_LULC_MERGE) {

  dfmain <- read.csv(file.path(ANALYSIS_DIR, "AF_climate.csv"))
  lulc   <- read.csv(file.path(ANALYSIS_DIR, "AF_lulc_metrics.csv"))

  # Map long site names to short field names used in the yield dataset
  lulc <- lulc |>
    mutate(field_short = recode(field, !!!field_name_map))

  dflulc <- dfmain %>%
    left_join(
      select(lulc, field_short, year, l_contag, l_np, l_sidi, l_shdi, l_ai, l_ed),
      by = c("field" = "field_short", "year"),
      relationship = "many-to-one"
    )

  write_csv(dflulc, file.path(ANALYSIS_DIR, "AF_lulc.csv"))
  message("\nStep 4 complete — AF_lulc.csv  (", nrow(dflulc), " rows)")
}
