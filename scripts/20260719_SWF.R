### SWF

library(terra)
library(sf)
library(tidyverse)


# field polygons:
fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")


# Reprojection ------------------------------------------------------------

# reprojection
fieldpolygons_3035 <- st_as_sf(fieldpolygons, coords = c("minLongitude","minLatitude"), crs = 4326) %>%
  st_transform(3035)

# Derive tile name from LAEA coordinates
# tile origin = floor(coord / 100000) → gives the E/N index
tile_E <- floor(coords[,"X"] / 100000)
tile_N <- floor(coords[,"Y"] / 100000)
tile_names <- sprintf("E%02dN%02d", tile_E, tile_N)
data.frame(site = fieldpolygons$Name, tile = tile_names)

# SWF 2021  -------------------------------------------------------------

# load raster 
tif_files <- list.files(".\\data\\SWF\\2021", pattern = "\\.tif$", full.names = TRUE)
r <- terra::rast(tif_files)
crs(r)

# reproject 
fieldpolygons_proj <- fieldpolygons %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(r))

coords <- st_coordinates(fieldpolygons_proj)

# buffer 1 km
fields_buf <- st_buffer(fieldpolygons_proj, dist = 1000)


# clip raster 
site_col <- "Name"   # ← change to whatever your site name column is called

for (i in seq_len(nrow(fields_buf))) {
  
  site_name <- fields_buf[[site_col]][i]
  cat("Processing:", site_name, "\n")
  
  # convert sf polygon to SpatVector for terra
  poly <- vect(fields_buf[i, ])
  
  # crop to bounding box first (fast), then mask to exact polygon shape
  r_crop <- crop(r, poly)
  r_mask <- mask(r_crop, poly)
  
  # clean filename — removes spaces and special characters
  fname <- paste0("output/SWF/2021/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m.tif")
  
  writeRaster(r_mask, fname, overwrite = TRUE)
  cat("  Saved to:", fname, "\n")
  plot(r_mask, main = as.character(fname))
}

# plot it 
r <- rast("output/Dornburg_field_buf1000m.tif")
plot(r)

# SWF 2018  -------------------------------------------------------------

rm(list=ls())

# load raster 
tif_files <- list.files(".\\data\\SWF\\2018", pattern = "\\.tif$", full.names = TRUE)
r <- vrt(tif_files) 

# load field polygons:
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

for (i in seq_len(nrow(fields_buf))) {
  
  site_name <- fields_buf[[site_col]][i]
  cat("Processing:", site_name, "\n")
  
  # convert sf polygon to SpatVector for terra
  poly <- vect(fields_buf[i, ])
  
  # crop to bounding box first (fast), then mask to exact polygon shape
  r_crop <- crop(r, poly)
  r_mask <- mask(r_crop, poly)
  
  # clean filename — removes spaces and special characters
  fname <- paste0("output/SWF/2018/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m.tif")
  
  writeRaster(r_mask, fname, overwrite = TRUE)
  cat("  Saved to:", fname, "\n")
  plot(r_mask, main = as.character(fname))
}

# plot it 
r <- rast("output/2018/Mariensee_Field_buf1000m.tif")
plot(r)

# SWF 2015  -------------------------------------------------------------

rm(list=ls())

# ── Option 1: use gdal_translate via terra's gdal wrapper ────────────────────
library(terra)

infile  <- ".\\data\\SWF\\2015\\swf_2015_005m_DE029_3035_v012\\Data\\swf_2015_005m_DE029_3035_v012.tif"

# check what overviews exist
gdal_utils("info", infile)   # look for "Overviews:" in the output

r_full <- rast(infile)

# tell terra explicitly which overview level — level 0 = full resolution
# this is the cleanest fix for newer terra versions
r_full <- rast(infile, lyrs = 1)   

# check
res(r_full)

# load ONLY the swf tif files, not the quality layer
tif_files <- list.files(
  path       = ".\\data\\SWF\\2015",
  pattern    = "^swf_.*\\.tif$",   # starts with swf_, not HRL_
  full.names = TRUE,
  recursive  = TRUE
)

print(tif_files)   # confirm only the 7 DE swf files are listed
r <- vrt(tif_files)
res(r)

# load raster 

tif_files <- list.files(".\\data\\SWF\\2015", pattern = "\\.tif$", full.names = TRUE)
r <-  terra::vrt(tif_files, lyrs = 1) 
# what resolution is terra actually seeing?
res(r)          # should be c(5, 5) for a 5m product
# if you see c(200, 200) or similar → overview problem

# check the actual file being read
sources(r)      # lists which files are in the vrt
plot(r)
# load field polygons:
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
# after crop and mask per field:
swf_summary <- function(r_masked) {
  v <- values(r_masked)
  v <- v[!is.na(v)]
  v <- v[v != 0 & v != 254]   # exclude background and cloud
  data.frame(
    n_pixels    = length(v),
    n_swf       = sum(v == 1),
    n_unclear   = sum(v == 3),
    pct_swf     = round(sum(v == 1) / length(v) * 100, 2)
  )
}
for (i in seq_len(nrow(fields_buf))) {
  
  site_name <- fields_buf[[site_col]][i]
  cat("Processing:", site_name, "\n")
  
  # convert sf polygon to SpatVector for terra
  poly <- vect(fields_buf[i, ])
  
  # crop to bounding box first (fast), then mask to exact polygon shape
  r_crop <- crop(r, poly)
  r_mask <- mask(r_crop, poly)
  
  # clean filename — removes spaces and special characters
  fname <- paste0("output/SWF/2015/", gsub("[^a-zA-Z0-9]", "_", site_name), "_buf1000m.tif")
  
  writeRaster(r_mask, fname, overwrite = TRUE)
  cat("  Saved to:", fname, "\n")
  plot(r_mask, main = as.character(fname))
  
  swfsum <- swf_summary(r_mask)
  print(swfsum)
}

# plot it 
r <- rast("output/2015/Dornburg_Field_buf1000m.tif")
plot(r)



# check if something missing: ----------------------------------------------
# check one crop before masking
poly_test <- vect(fields_buf[1, ])
r_crop_test <- crop(r, poly_test)

minmax(r_crop_test)
freq(r_crop_test)        # do you see real class values here?
plot(r_crop_test)        # visual check — does it look right before masking?

# now check after mask
r_mask_test <- mask(r_crop_test, poly_test)
freq(r_mask_test)        # do values survive the mask step?
plot(r_mask_test)


# consequesnt processing 26/7/2026

# 2018
raster_dir <- "output/swf/2018"
raster_files <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)
r <- terra::rast(raster_files[1])
print(r)
plot(r)

# 2015
raster_dir <- "output/swf/2015"
raster_files <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)
r <- terra::rast(raster_files[1])
print(r)
plot(r)
r[r == 3] <- 1
r[r != 1] <- 0

for (i in seq_along(raster_files)) {
  r <- terra::rast(raster_files[i])
  r[r == 3] <- 1
  r[r != 1] <- 0
  fname <- paste0("output/SWF/2015/", sub(".*/2015/(.*)\\.tif$", "\\1", raster_files[i]), "_binary.tif")
  terra::writeRaster(r, fname, overwrite = TRUE)
}
