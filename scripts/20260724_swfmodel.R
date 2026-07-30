library(raster)
library(sp)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(terra)
library(data.table)

tif_files <- list.files(".\\data\\SWF\\2021", pattern = "\\.tif$", full.names = TRUE)
woody_map <- terra::rast(tif_files)

fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")
fields_sf <- st_as_sf(fieldpolygons, coords = c("Longitude","Latitude"), crs = 4326) %>%
  st_transform(crs(woody_map))

raster_dir <- "output/swf/2021"
raster_files <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)

field_names <- gsub("_.*", "", basename(raster_files))

max_radius <- 500
radius_step <- 50
annulus_radii <- seq(radius_step, max_radius, by = radius_step)

extract_annulus_profile <- function(point_coords, raster, radii, step) {
  results <- data.frame(radius = numeric(), prop_noncrop = numeric())
  
  for (r in radii) {
    r_inner <- r - step
    r_outer <- r
    
    buffer_outer <- st_buffer(point_coords, dist = r_outer)
    buffer_inner <- st_buffer(point_coords, dist = r_inner)
    
    annulus_mask <- st_difference(buffer_outer, buffer_inner)
    annulus_vect <- vect(annulus_mask)
    
    vals_annulus <- terra::extract(raster, annulus_vect)
    
    if (!is.null(vals_annulus) && length(vals_annulus[[2]]) > 0) {
      annulus_values <- vals_annulus[[2]]
      annulus_values <- annulus_values[!is.na(annulus_values)]
      
      if (length(annulus_values) > 0) {
        prop <- sum(annulus_values) / length(annulus_values)
      } else {
        prop <- NA
      }
    } else {
      prop <- NA
    }
    
    results <- rbind(results, data.frame(radius = r, prop_swf = prop))
  }
  
  return(results)
}

field_profiles <- list()
fields_sf$id <- c("Ihinger", "Mariensee", "Gladbacherhof", "Forst", "Dornburg",
                  "Wendhausen", "Vechta", "Reiffenhausen")
for (i in seq_along(raster_files)) {
  woody_map_crop <- rast(raster_files[i])
  woody_binary <- as.numeric(woody_map_crop)
  
  field_name <- field_names[i]
  field_row <- fields_sf[fields_sf$id == field_name, ]
  
  if (nrow(field_row) > 0) {
    profile <- extract_annulus_profile(
      st_geometry(field_row),
      woody_binary,
      annulus_radii,
      radius_step
    )
    profile$id <- field_name
    field_profiles[[i]] <- profile
    print(paste("Processed:", field_name))
  }
}

profiles_df <- do.call(rbind, field_profiles)
rownames(profiles_df) <- NULL

print(profiles_df)



# visulas -----------------------------------------------------------------

profiles_df %>% group_by(id) %>% 
  ggplot(aes(x = radius, y = prop_swf, colour = id))+
  #labs(ylim=c(0:500))+
  geom_smooth()
