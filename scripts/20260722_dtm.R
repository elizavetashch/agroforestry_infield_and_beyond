
rm(list=ls())

# Packages  ---------------------------------------------------------------

library(ncdf4)
library(terra)
library(tidyverse)
library(sf)
library(readr)


# Data --------------------------------------------------------------------

path <- "data/dtm/srtm_germany_dtm.tif"
r <- rast(path)

print(r)          
nlyr(r)          
crs(r)     #  WGS 84 (EPSG:4326)      
ext(r)            

fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")

# -------------------------------------------------------------------------


# reproject 
fieldpolygons_proj <- fieldpolygons %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(r))

# buffer 1 km
fields_buf <- st_buffer(fieldpolygons_proj, dist = 1000)


# clip raster 
site_col <- "Name"   # ← change to whatever your site name column is called

# crop the raster for all the polygonns 
for (i in seq_len(nrow(fields_buf))) {
  site_name <- fields_buf[[site_col]][i]
  cat("Processing:", site_name, "\n")
  
  # convert sf polygon to SpatVector for terra
  poly <- vect(fields_buf[i, ])
  
  # crop to bounding box first (fast), then mask to exact polygon shape
  r_crop <- crop(r, poly)
  r_mask <- mask(r_crop, poly)
  
  # clean filename — removes spaces and special characters
  fname <- paste0("output/dtm/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m.tif")
  
  writeRaster(r_mask, fname, overwrite = TRUE)
  cat("  Saved to:", fname, "\n")
  # plot(r_mask, main = as.character(fname))
  
}

for (i in seq_len(nrow(fields_buf))) {
  site_name <- fields_buf[[site_col]][i]
  fname <- paste0("output/dtm/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m.tif")
  r <- terra::rast(fname)
  print(r)
  plot(r, main = as.character(site_name))
}

par(mfrow=c(1,1))
site_name <- "Gladbacherhof Field"
fname <- paste0("output/dtm/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m.tif")
r <- terra::rast(fname)
print(r)
plot(r, main = as.character(site_name))
