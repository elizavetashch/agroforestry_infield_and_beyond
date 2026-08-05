rm(list=ls())
library(raster)
library(sp)
library(sf)
library(dplyr)
library(tidyr)
library(terra)
library(geosphere) #distHaversine function 



# (1) Read the Data -------------------------------------------------------

fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")

fieldpolygons <- fieldpolygons %>% 
  mutate( 
    fieldlength = distHaversine( cbind(minLongitude, minLatitude), cbind(maxLongitude, maxLatitude) ),
    fieldlength = as.numeric(fieldlength)) 

fields_sf <- st_as_sf(fieldpolygons, coords = c("Longitude","Latitude"), crs = 4326) %>%
  st_transform(3035)


# (2) Extract Annulus Profile Function ------------------------------------


extract_annulus_profile <- function(point_coords, raster, radii) {
  results <- data.frame(radius = numeric(), prop_noncrop = numeric())
  
  for (idx in seq_along(radii)) {
    
    r_outer <- radii[idx]
    r_inner <- if (idx == 1) 0 else radii[idx - 1]
    
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
    
    results <- rbind(results, data.frame(radius = r_outer, prop_swf = prop))
  }
  
  return(results)
}



# (3.1) 2021  ---------------------------------------------------------------

# 2021
raster_dir <- "output/swf/2021"
raster_files <- list.files(raster_dir, pattern = "\\_buf3000m\\.tif$", full.names = TRUE)
field_names <- gsub("_.*", "", basename(raster_files))

field_profiles <- list()
fields_sf$id <- c("Ihinger", "Mariensee", "Gladbacherhof", "Forst", "Dornburg",
                  "Wendhausen", "Vechta", "Reiffenhausen")

for (i in seq_along(raster_files)) {
  woody_map_crop <- rast(raster_files[i])
  woody_binary <- as.numeric(woody_map_crop)
  
  field_name <- field_names[i]
  field_row <- fields_sf[fields_sf$id == field_name, ]
  
  max_radius <- ((round(fields_sf$fieldlength / 2 / 100) * 100)+1000)[which(fields_sf$id == field_name)]
  radius_step <- 200
  target_area <- pi * (radius_step)^2  # Area of first annulus
  
  annulus_radii <- numeric()
  r <- 0
  
  while (r < max_radius) {
    # For equal area: A = π(r_outer² - r_inner²) = target_area
    # Solving for r_outer: r_outer = sqrt(r_inner² + target_area/π)
    r <- sqrt(r^2 + target_area / pi)
    if (r <= max_radius) {
      annulus_radii <- c(annulus_radii, r)
    }
  }
  
  if (nrow(field_row) > 0) {
    profile <- extract_annulus_profile(
      st_geometry(field_row),
      woody_binary,
      annulus_radii
    )
    profile$id <- field_name
    field_profiles[[i]] <- profile
    print(paste("Processed:", field_name))
  }
}

profiles2021 <- do.call(rbind, field_profiles)
rownames(profiles2021) <- NULL
print(profiles2021)


# (3.2) 2018  ---------------------------------------------------------------

# 2018
rm(field_profiles, field_row, profile,woody_map_crop,woody_binary,
   raster_dir, raster_files)

raster_dir <- "output/swf/2018"
raster_files <- list.files(raster_dir, pattern = "\\_buf3000m\\.tif$", full.names = TRUE)
field_names <- gsub("_.*", "", basename(raster_files))

field_profiles <- list()
fields_sf$id <- c("Ihinger", "Mariensee", "Gladbacherhof", "Forst", "Dornburg",
                  "Wendhausen", "Vechta", "Reiffenhausen")

for (i in seq_along(raster_files)) {
  woody_map_crop <- rast(raster_files[i])
  woody_binary <- as.numeric(woody_map_crop)
  
  field_name <- field_names[i]
  field_row <- fields_sf[fields_sf$id == field_name, ]
  
  max_radius <- ((round(fields_sf$fieldlength / 2 / 100) * 100)+1000)[which(fields_sf$id == field_name)]
  radius_step <- 200
  target_area <- pi * (radius_step)^2  # Area of first annulus
  
  annulus_radii <- numeric()
  r <- 0
  
  while (r < max_radius) {
    # For equal area: A = π(r_outer² - r_inner²) = target_area
    # Solving for r_outer: r_outer = sqrt(r_inner² + target_area/π)
    r <- sqrt(r^2 + target_area / pi)
    if (r <= max_radius) {
      annulus_radii <- c(annulus_radii, r)
    }
  }
  
  
  if (nrow(field_row) > 0) {
    profile <- extract_annulus_profile(
      st_geometry(field_row),
      woody_binary,
      annulus_radii
    )
    profile$id <- field_name
    field_profiles[[i]] <- profile
    print(paste("Processed:", field_name))
  }
}

profiles2018 <- do.call(rbind, field_profiles)
rownames(profiles2018) <- NULL
print(profiles2018)



# (3.3) 2015 --------------------------------------------------------------


# 2015
rm(field_profiles, field_row, profile,woody_map_crop,woody_binary,
   raster_dir, raster_files)

raster_dir <- "output/swf/2015"
raster_files <- list.files(raster_dir, pattern = "\\_buf3000m_binary.tif$", full.names = TRUE)
field_names <- gsub("_.*", "", basename(raster_files))

field_profiles <- list()
fields_sf$id <- c("Ihinger", "Mariensee", "Gladbacherhof", "Forst", "Dornburg",
                  "Wendhausen", "Vechta", "Reiffenhausen")

for (i in seq_along(raster_files)) {
  woody_map_crop <- rast(raster_files[i])
  woody_binary <- as.numeric(woody_map_crop)
  
  field_name <- field_names[i]
  field_row <- fields_sf[fields_sf$id == field_name, ]
  
  max_radius <- ((round(fields_sf$fieldlength / 2 / 100) * 100)+1000)[which(fields_sf$id == field_name)]
  radius_step <- 200
  target_area <- pi * (radius_step)^2  # Area of first annulus
  
  annulus_radii <- numeric()
  r <- 0
  
  while (r < max_radius) {
    # For equal area: A = π(r_outer² - r_inner²) = target_area
    # Solving for r_outer: r_outer = sqrt(r_inner² + target_area/π)
    r <- sqrt(r^2 + target_area / pi)
    if (r <= max_radius) {
      annulus_radii <- c(annulus_radii, r)
    }
  }
  
  
  if (nrow(field_row) > 0) {
    profile <- extract_annulus_profile(
      st_geometry(field_row),
      woody_binary,
      annulus_radii
    )
    profile$id <- field_name
    field_profiles[[i]] <- profile
    print(paste("Processed:", field_name))
  }
}

profiles2015 <- do.call(rbind, field_profiles)
rownames(profiles2015) <- NULL
print(profiles2015)


# (4.1.) Merge ---------------------------------------------------------------


# merge 
swf <- merge(profiles2015, profiles2018, by = c("id", "radius"), all.x = TRUE, suffixes = c(".2015",".2018"))
swf <- merge(swf, profiles2021, by = c("id", "radius"), all.x = TRUE)
names(swf)[names(swf) == "prop_swf"] <- "prop_swf.2021"
head(swf)

write.csv(swf, "./output/swf/20260805_dfswf.csv", row.names = FALSE)



# (4.2.) Merge to the Dataset  --------------------------------------------

# merge with the dataset 
rm(list=ls())

swf <- read.csv("./output/swf/20260805_dfswf.csv")
df <-  read.csv("data/AnalysisData/20260803_AFdistance.csv")

levels(as.factor(swf$id))
levels(as.factor(df$field))

swf$id <- gsub("Ihinger", "IhingerHof", swf$id)

df <- df %>%
  mutate(swf_year = case_when(
    year == 2016 ~ "prop_swf.2015",
    year %in% 2017:2019 ~ "prop_swf.2018",
    year >= 2020 ~ "prop_swf.2021"
  ))


 swf_long <- 
  swf %>%
  pivot_longer(
    starts_with("prop_swf."),
    names_to = "swf_year",
    values_to = "prop_swf"
  ) 

dfswf <- df %>%
  left_join(
    swf_long,
    by = c("field" = "id", "swf_year"),
    relationship = "many-to-many"
  )

table(swf$id)

write.csv(dfswf, "./data/AnalysisData/20260805_AFswf.csv", row.names = FALSE)


# (5) Visualize -----------------------------------------------------------

library(ggplot2)
library(sf)

# Create equal-area annuli circles for visualization
create_annuli_circles <- function(center_point, radii, n_points = 360) {
  circles <- list()
  
  for (i in seq_along(radii)) {
    radius <- radii[i]
    angles <- seq(0, 2*pi, length.out = n_points)
    
    x <- center_point[1] + radius * cos(angles)
    y <- center_point[2] + radius * sin(angles)
    
    circles[[i]] <- data.frame(
      x = x,
      y = y,
      radius = radius,
      annulus = i
    )
  }
  
  return(do.call(rbind, circles))
}

# Get center and radii for a field
field_center <- st_coordinates(st_geometry(fields_sf[4, ]))  # First field
swf[swf$id=="Ihinger", 2]

radii <- swf[swf$id=="Ihinger", 2]  # Your equal-area radii vector
ihingerraster <- rast("output/swf/2021/Ihinger_Hof_Field_buf1000m.tif")
ihingerraster_df <- as.data.frame(ihingerraster, xy = TRUE)
colnames(ihingerraster_df) <- c("x", "y", "value")
# Create circles data
circles_df <- create_annuli_circles(field_center, radii)
zoom_radius <- 500
# Create the plot
ggplot() +
  # Plot your raster (non-crop in dark, crop in yellow)
  geom_raster(data = ihingerraster_df, aes(x = x, y = y, fill = value)) +
  scale_fill_manual(values = c("Non SWF area" = "lightgrey", "SWF area" = "green")) +
  
  # Overlay the annuli circles
  geom_path(data = circles_df, aes(x = x, y = y, group = annulus), 
            color = "black", linetype = "dashed", linewidth = 0.5) +
  
  # Add radius labels
  geom_text(data = circles_df %>% 
              group_by(annulus) %>% 
              slice(1) %>% 
              ungroup(),
            aes(x = x, y = y, label = paste0(round(radius), "m")),
            color = "white", size = 3) +
  
  # Field boundary
  geom_sf(data = fields_sf[1, ], fill = NA, color = "red", linewidth = 1) +
  
  # Zoom in to specific radius
  coord_sf(xlim = c(field_center[1] - zoom_radius, field_center[1] + zoom_radius),
           ylim = c(field_center[2] - zoom_radius, field_center[2] + zoom_radius)) +
  
  theme_minimal() +
  labs(title = "Equal-area annuli around field center",
       fill = "Land cover") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
