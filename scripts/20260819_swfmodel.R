rm(list = ls())
library(raster)
library(sp)
library(sf)
library(dplyr)
library(tidyr)
library(terra)
library(geosphere)  # distHaversine()

# ============================================================================
# (1) Read the Data
# ============================================================================

fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")

fieldpolygons <- fieldpolygons %>%
  mutate(
    fieldlength = distHaversine(cbind(minLongitude, minLatitude), cbind(maxLongitude, maxLatitude)),
    fieldlength = as.numeric(fieldlength),
    boundary_min = fieldlength / 2          # field boundary = radius from center
  )

fields_sf <- st_as_sf(fieldpolygons, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(3035)

DISTANCES <- seq(100, 1000, by = 100)        # distances FROM the boundary, 100m steps

# ============================================================================
# (2) Extraction functions
# ============================================================================

# ---- annuli starting at the field boundary, stepping outward every 100m ----
extract_annulus_profile <- function(point_coords, raster, boundary_min, distances = DISTANCES) {
  
  results <- data.frame(distance = numeric(), radius = numeric(), prop_swf = numeric())
  
  for (idx in seq_along(distances)) {
    
    r_outer <- boundary_min + distances[idx]
    r_inner <- if (idx == 1) boundary_min else boundary_min + distances[idx - 1]
    
    buffer_outer <- st_buffer(point_coords, dist = r_outer)
    buffer_inner <- st_buffer(point_coords, dist = r_inner)
    
    annulus_mask <- st_difference(buffer_outer, buffer_inner)
    annulus_vect <- vect(annulus_mask)
    
    vals_annulus <- terra::extract(raster, annulus_vect)
    
    if (!is.null(vals_annulus) && length(vals_annulus[[2]]) > 0) {
      annulus_values <- vals_annulus[[2]]
      annulus_values <- annulus_values[!is.na(annulus_values)]
      prop <- if (length(annulus_values) > 0) sum(annulus_values) / length(annulus_values) else NA
    } else {
      prop <- NA
    }
    
    results <- rbind(results, data.frame(distance = distances[idx], radius = r_outer, prop_swf = prop))
  }
  
  return(results)
}

# ---- single disk covering the field itself (radius = boundary_min) ----
extract_field_interior <- function(point_coords, raster, boundary_min) {
  
  buffer_field <- st_buffer(point_coords, dist = boundary_min)
  field_vect <- vect(buffer_field)
  
  vals_field <- terra::extract(raster, field_vect)
  
  if (!is.null(vals_field) && length(vals_field[[2]]) > 0) {
    field_values <- vals_field[[2]]
    field_values <- field_values[!is.na(field_values)]
    prop <- if (length(field_values) > 0) sum(field_values) / length(field_values) else NA
  } else {
    prop <- NA
  }
  
  data.frame(radius = boundary_min, prop_swf = prop)
}

# ---- process one year's rasters: returns list(annulus = ..., interior = ...) ----
process_year <- function(raster_dir, pattern, fields_sf, field_ids) {
  
  raster_files <- list.files(raster_dir, pattern = pattern, full.names = TRUE)
  field_names  <- gsub("_.*", "", basename(raster_files))
  fields_sf$id <- field_ids
  
  annulus_list  <- list()
  interior_list <- list()
  
  for (i in seq_along(raster_files)) {
    woody_map_crop <- rast(raster_files[i])
    woody_binary   <- as.numeric(woody_map_crop)
    
    field_name <- field_names[i]
    field_row  <- fields_sf[fields_sf$id == field_name, ]
    if (nrow(field_row) == 0) next
    
    boundary_min <- field_row$boundary_min[1]
    point_coords <- st_geometry(field_row)
    
    ann_profile <- extract_annulus_profile(point_coords, woody_binary, boundary_min)
    ann_profile$id <- field_name
    annulus_list[[field_name]] <- ann_profile
    
    int_profile <- extract_field_interior(point_coords, woody_binary, boundary_min)
    int_profile$id <- field_name
    interior_list[[field_name]] <- int_profile
    
    print(paste("Processed:", field_name))
  }
  
  list(
    annulus  = do.call(rbind, annulus_list)  %>% `rownames<-`(NULL),
    interior = do.call(rbind, interior_list) %>% `rownames<-`(NULL)
  )
}

field_ids <- c("Ihinger", "Mariensee", "Gladbacherhof", "Forst", "Dornburg",
               "Wendhausen", "Vechta", "Reiffenhausen")

# ============================================================================
# (3) Run for each year
# ============================================================================

res2021 <- process_year("output/swf/2021", "\\_buf3000m\\.tif$",        fields_sf, field_ids)
res2018 <- process_year("output/swf/2018", "\\_buf3000m\\.tif$",        fields_sf, field_ids)
res2015 <- process_year("output/swf/2015", "\\_buf3000m_binary.tif$",   fields_sf, field_ids)

# ============================================================================
# (4) Merge across years and write the two CSVs
# ============================================================================

# ---- CSV 1: annulus profiles, distance (100-1000m) from the field boundary ----
annulus_all <- res2015$annulus %>%
  rename(prop_swf.2015 = prop_swf) %>%
  full_join(res2018$annulus %>% rename(prop_swf.2018 = prop_swf),
            by = c("id", "distance"), suffix = c("", "")) %>%
  full_join(res2021$annulus %>% rename(prop_swf.2021 = prop_swf),
            by = c("id", "distance"), suffix = c("", "")) %>%
  dplyr::select(id, distance, starts_with("radius"), starts_with("prop_swf")) %>%
  arrange(id, distance)

write.csv(annulus_all, "./output/swf/20260819_swf_annulus_by_distance.csv", row.names = FALSE)

# ---- CSV 2: proportion of SWF within the field itself (disk of radius = boundary_min) ----
interior_all <- res2015$interior %>%
  dplyr::select(id, prop_swf.2015 = prop_swf) %>%
  full_join(res2018$interior %>% dplyr::select(id, prop_swf.2018 = prop_swf), by = "id") %>%
  full_join(res2021$interior %>% dplyr::select(id, prop_swf.2021 = prop_swf), by = "id") %>%
  arrange(id)

write.csv(interior_all, "./output/swf/20260819_swf_within_field.csv", row.names = FALSE)

print(annulus_all)
print(interior_all)

annulus_all |> 
  ggplot(aes(x = distance, y = prop_swf.2015, color = id))+
  geom_line()+
  theme_bw()


#  Merge to the Dataset  --------------------------------------------

# merge with the dataset 
rm(list=ls())

swf <- read.csv("./output/swf/20260819_swf_annulus_by_distance.csv")
intrinsic <- read.csv("./output/swf/20260819_swf_within_field.csv")
df <-  read.csv("data/AnalysisData/20260805_AFslope.csv")

df <- df |> dplyr::select(-c(prop_swf, swf_year, radius))

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

intrinsic_long  <- 
  intrinsic  %>%
  pivot_longer(
    starts_with("prop_swf."),
    names_to = "swf_year",
    values_to = "prop_swf_within"
  ) 

dfswf_intr <- dfswf %>%
  left_join(
    intrinsic_long,
    by = c("field" = "id", "swf_year"),
    relationship = "many-to-many"
  )


write.csv(dfswf_intr, "./data/AnalysisData/20260819_AFswf.csv", row.names = FALSE)

