rm(list=ls())
library(raster)
library(sp)
library(sf)
library(dplyr)
library(tidyr)
library(terra)

# function to extract the profiles 
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

# fieldpolygons
fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")
fields_sf <- st_as_sf(fieldpolygons, coords = c("Longitude","Latitude"), crs = 4326) %>%
  st_transform(3035)

# 2021
raster_dir <- "output/swf/2021"
raster_files <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)
field_names <- gsub("_.*", "", basename(raster_files))

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

profiles2021 <- do.call(rbind, field_profiles)
rownames(profiles2021) <- NULL

print(profiles2021)


# 2018
rm(field_profiles, field_row, profile,woody_map_crop,woody_binary,
   raster_dir, raster_files)

raster_dir <- "output/swf/2018"
raster_files <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)
field_names <- gsub("_.*", "", basename(raster_files))

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

profiles2018 <- do.call(rbind, field_profiles)
rownames(profiles2018) <- NULL

print(profiles2018)


# 2015
rm(field_profiles, field_row, profile,woody_map_crop,woody_binary,
   raster_dir, raster_files)

raster_dir <- "output/swf/2015"
raster_files <- list.files(raster_dir, pattern = "\\_binary.tif$", full.names = TRUE)
field_names <- gsub("_.*", "", basename(raster_files))

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

profiles2015 <- do.call(rbind, field_profiles)
rownames(profiles2015) <- NULL

print(profiles2015)

# merge 
swf <- merge(profiles2015, profiles2018, by = c("id", "radius"), all.x = TRUE, suffixes = c(".2015",".2018"))
swf <- merge(swf, profiles2021, by = c("id", "radius"), all.x = TRUE)
names(swf)[names(swf) == "prop_swf"] <- "prop_swf.2021"
head(swf)
write.csv(swf, "./output/swf/swf.csv", row.names = FALSE)

# merge with the dataset 
rm(list=ls())
swf <- read.csv("./output/swf/swf.csv")
df <-  read.csv("data/AnalysisData/20260723_all_fields.csv")
colnames(df)
shortdf <- df %>% select(data_id, field, plot, yield_tha, year, distance_to_tree_strip, tree_species, crop_unified)

fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")
levels(as.factor(fieldpolygons$Name))
levels(as.factor(shortdf$field))

fieldpolygons$field <- gsub(" Field| field", "", fieldpolygons$Name)
fieldpolygons$field <- gsub("Ihinger Hof", "IhingerHof", fieldpolygons$field)
fieldpolygonsmerge <- fieldpolygons %>% select(field, Latitude, Longitude, minLatitude, minLongitude, maxLatitude, maxLongitude, Area)

df <- merge(shortdf, fieldpolygonsmerge, by = "field", all.x = TRUE)
colnames(df)
head(df)

levels(as.factor(df$field))
levels(as.factor(swf$id))
swf$id <- gsub("Ihinger", "IhingerHof", swf$id)

df <- df %>%
  mutate(swf_year = case_when(
    year == 2016 ~ "prop_swf.2015",
    year %in% 2017:2019 ~ "prop_swf.2018",
    year >= 2020 ~ "prop_swf.2021"
  ))

swf_long <- swf %>%
  tidyr::pivot_longer(
    starts_with("prop_swf."),
    names_to = "swf_year",
    values_to = "prop_swf"
  )

dfswf <- df %>%
  left_join(
    swf_long,
    by = c("field" = "id", "swf_year")
  )

write.csv(dfswf, "./analysis_data/20260726_propswf.csv", row.names = FALSE)
