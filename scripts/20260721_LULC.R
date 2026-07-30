rm(list=ls())


library(ncdf4)
library(terra)
library(tidyverse)
library(sf)
library(readr)

# field polygons
# load field polygons:
fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")


# raster 
r2017 <- terra::rast("data/LULC/2017/32U_20170101-20180101.tif")
r2018 <- terra::rast("data/LULC/2018/32U_20180101-20190101.tif")
r2019 <- terra::rast("data/LULC/2019/32U_20190101-20200101.tif")
r2020 <- terra::rast("data/LULC/2020/32U_20200101-20210101.tif")
r2021 <- terra::rast("data/LULC/2021/32U_20210101-20220101.tif")
r2022 <- terra::rast("data/LULC/2022/32U_20220101-20230101.tif")
r2023 <- terra::rast("data/LULC/2023/32U_20230101-20240101.tif")

lulc_legend <- data.frame(
  value = c(1, 2, 4, 5, 7, 8, 9, 10, 11),
  class = c(
    "Water",
    "Trees",
    "Flooded Vegetation",
    "Crops",
    "Built Area",
    "Bare Ground",
    "Snow/Ice",
    "Clouds",
    "Rangeland"
  )
)


# reproject 
fieldpolygons_proj <- fieldpolygons %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(r2017))

# buffer 1 km
fields_buf <- st_buffer(fieldpolygons_proj, dist = 1000)
fields_buf_noforst <- fields_buf %>% filter(Name != "Forst Field")


rasters <- list(r2017,r2018,r2019, r2020, r2021, r2022,r2023)

# clip raster 
site_col <- "Name"   # ← change to whatever your site name column is called


for (i in seq_len(nrow(fields_buf_noforst))) {
  for (year_idx in seq_along(rasters)) {
  year <- 2016 + year_idx
  site_name <- fields_buf_noforst[[site_col]][i]
  
  cat("Processing:", site_name, "\n")
  
  # convert sf polygon to SpatVector for terra
  poly <- vect(fields_buf_noforst[i, ])
  
  # crop to bounding box first (fast), then mask to exact polygon shape
  levels(rasters[[year_idx]]) <- lulc_legend
  r_crop <- crop(rasters[[year_idx]], poly)
  r_mask <- mask(r_crop, poly)
  
  # clean filename — removes spaces and special characters
  fname <- paste0("output/LULC/", gsub("[^a-zA-Z0-9]", "_", site_name),"_", year, ".tif")
  
  writeRaster(r_mask, fname, overwrite = TRUE)
  cat("  Saved to:", fname, "\n")
  
  }
}
years <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023)


for (i in seq_len(nrow(fields_buf_noforst))) {
  par(mfrow=c(2, 4))
  for (year in years){
  site_name <- fields_buf_noforst[[site_col]][i]
  cat("Processing:", site_name, "\n")
  fname <- paste0("output/LULC/", gsub("[^a-zA-Z0-9]", "_", site_name),"_", year, ".tif")
  raster <- terra::rast(fname)
  levels(raster) <- lulc_legend
  plot(raster, main = paste0(site_name," ", year))
}
}
