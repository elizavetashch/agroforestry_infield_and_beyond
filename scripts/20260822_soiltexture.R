
library(terra)
library(sf)

tif_files <- list.files(".\\data\\SoilGrids", pattern = "\\.tif$", full.names = TRUE)
r <- terra::rast(tif_files)
crs(r)

fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")

fieldpolygons_proj <- fieldpolygons %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(r))

coords <- st_coordinates(fieldpolygons_proj)
 
values <- extract(r, coords, method = "simple")

fields <- c("IhingerHof", "Mariensee", "Gladbacherhof", "Forst", "Dornburg", "Wendhausen", "Vechta", "Reiffenhausen")

result <- cbind(fields, values)

write.csv(result, ".\\data\\SoilGrids\\20260822_soiltexture.csv", row.names = FALSE)


  