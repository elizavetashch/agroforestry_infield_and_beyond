


rm(list=ls())

# Packages  ---------------------------------------------------------------

library(ncdf4)
library(terra)
library(tidyverse)
library(sf)
library(readr)


# Windspeed 2016 -------------------------------------------------------------

#open a netCDF file 
# wind speed: 
nc_file <- nc_open("data/climate/MPI_CLM_R85_wsp_cor_2016_v2.nc")
print(nc_file)

path <- "data/climate/MPI_CLM_R85_wsp_cor_2016_v2.nc"
r <- rast(path, subds = "wsp_cor")

# Basic metadata
print(r)          # dimensions, resolution, CRS, value range
nlyr(r)           # should be 365 (one layer per day)
crs(r)            # UTM32N
ext(r)            # spatial extent in metres


crs(r) <- "EPSG:25832"   # ETRS89 / UTM zone 32N

fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")

# reproject 
fieldpolygons_proj <- fieldpolygons %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(r))

coords <- st_coordinates(fieldpolygons_proj)

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
  fname <- paste0("output/climate/windspeed/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m.tif")
  
  writeRaster(r_mask, fname, overwrite = TRUE)
  cat("  Saved to:", fname, "\n")
  # plot(r_mask, main = as.character(fname))

}

for (i in seq_len(nrow(fields_buf))) {
  site_name <- fields_buf[[site_col]][i]
  fname <- paste0("output/climate/windspeed/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m.tif")
  r <- terra::rast(fname)
  print(r)
  plot(r[[150]], main = as.character(site_name))
}




# Mean temperature 2016 -------------------------------------------------------------

path <- "data/climate/MPIR85_MeanTemperature/MPI_CLM_R85_tav_cor_2016_v2.nc"
nc_file <- nc_open(path)
print(nc_file)
crs(r)
r <- rast(path, subds = "tav_cor")

# Basic metadata
print(r)          # dimensions, resolution, CRS, value range
nlyr(r)           # should be 365 (one layer per day)
crs(r)            # UTM32N
ext(r)            # spatial extent in metres


fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")

# reproject 
fieldpolygons_proj <- fieldpolygons %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(r))

coords <- st_coordinates(fieldpolygons_proj)

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
  fname <- paste0("output/climate/meantemperature/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m_2016.tif")
  
  writeRaster(r_mask, fname, overwrite = TRUE)
  cat("  Saved to:", fname, "\n")
  # plot(r_mask, main = as.character(fname))
  
}

for (i in seq_len(nrow(fields_buf))) {
  site_name <- fields_buf[[site_col]][i]
  fname <- paste0("output/climate/meantemperature/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m_2016.tif")
  r <- terra::rast(fname)
  print(r)
  plot(r[[150]], main = as.character(site_name))
}

# global radiation --------------------------------------------------------

rm(list=ls())
path <- "data/climate/MPIR85_GlobalRadiation/MPI_CLM_R85_sgz_cor_2016_v2.nc"
nc_file <- nc_open(path)
print(nc_file)
r <- rast(path, subds = "sgz_cor")
print(r)
crs(r)

fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")


fieldpolygons_proj <- fieldpolygons %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(r))

coords <- st_coordinates(fieldpolygons_proj)

fields_buf <- st_buffer(fieldpolygons_proj, dist = 1000)

site_col <- "Name"   

for (i in seq_len(nrow(fields_buf))) {
  site_name <- fields_buf[[site_col]][i]
  cat("Processing:", site_name, "\n")
  
  # convert sf polygon to SpatVector for terra
  poly <- vect(fields_buf[i, ])
  
  # crop to bounding box first (fast), then mask to exact polygon shape
  r_crop <- crop(r, poly)
  r_mask <- mask(r_crop, poly)
  
  # clean filename — removes spaces and special characters
  fname <- paste0("output/climate/globalradiation/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m_2016.tif")
  
  writeRaster(r_mask, fname, overwrite = TRUE)
  cat("  Saved to:", fname, "\n")
  # plot(r_mask, main = as.character(fname))
  
}

for (i in seq_len(nrow(fields_buf))) {
  site_name <- fields_buf[[site_col]][i]
  fname <- paste0("output/climate/globalradiation/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m_2016.tif")
  r <- terra::rast(fname)
  print(r)
  plot(r[[150]], main = as.character(site_name))
}

# precipitation --------------------------------------------------------

rm(list=ls())
path <- "data/climate/MPIR85_Precipitation/MPI_CLM_R85_prz_cor_2016_v2.nc"
nc_file <- nc_open(path)
print(nc_file)
r <- rast(path, subds = "prz_cor")
print(r)
crs(r)

fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")


fieldpolygons_proj <- fieldpolygons %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(r))

coords <- st_coordinates(fieldpolygons_proj)

fields_buf <- st_buffer(fieldpolygons_proj, dist = 1000)

site_col <- "Name"   

for (i in seq_len(nrow(fields_buf))) {
  site_name <- fields_buf[[site_col]][i]
  cat("Processing:", site_name, "\n")
  
  # convert sf polygon to SpatVector for terra
  poly <- vect(fields_buf[i, ])
  
  # crop to bounding box first (fast), then mask to exact polygon shape
  r_crop <- crop(r, poly)
  r_mask <- mask(r_crop, poly)
  
  # clean filename — removes spaces and special characters
  fname <- paste0("output/climate/precipitation/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m_2016.tif")
  
  writeRaster(r_mask, fname, overwrite = TRUE)
  cat("  Saved to:", fname, "\n")
  # plot(r_mask, main = as.character(fname))
  
}

for (i in seq_len(nrow(fields_buf))) {
  site_name <- fields_buf[[site_col]][i]
  fname <- paste0("output/climate/precipitation/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m_2016.tif")
  r <- terra::rast(fname)
  print(r)
  plot(r[[150]], main = as.character(site_name))
}

# relative humidity --------------------------------------------------------

rm(list=ls())
path <- "data/climate/MPIR85_RelativeHumidity/MPI_CLM_R85_rhm_cor_2016_v2.nc"
nc_file <- nc_open(path)
print(nc_file)
r <- rast(path, subds = "rhm_cor")
print(r)
crs(r)

fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")


fieldpolygons_proj <- fieldpolygons %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(r))

coords <- st_coordinates(fieldpolygons_proj)

fields_buf <- st_buffer(fieldpolygons_proj, dist = 1000)

site_col <- "Name"   

for (i in seq_len(nrow(fields_buf))) {
  site_name <- fields_buf[[site_col]][i]
  cat("Processing:", site_name, "\n")
  
  # convert sf polygon to SpatVector for terra
  poly <- vect(fields_buf[i, ])
  
  # crop to bounding box first (fast), then mask to exact polygon shape
  r_crop <- crop(r, poly)
  r_mask <- mask(r_crop, poly)
  
  # clean filename — removes spaces and special characters
  fname <- paste0("output/climate/relativehumidity/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m_2016.tif")
  
  writeRaster(r_mask, fname, overwrite = TRUE)
  cat("  Saved to:", fname, "\n")
  # plot(r_mask, main = as.character(fname))
  
}

for (i in seq_len(nrow(fields_buf))) {
  site_name <- fields_buf[[site_col]][i]
  fname <- paste0("output/climate/relativehumidity/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m_2016.tif")
  r <- terra::rast(fname)
  print(r)
  plot(r[[150]], main = as.character(site_name))
}
