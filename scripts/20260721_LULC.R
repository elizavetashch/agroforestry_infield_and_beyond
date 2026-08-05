rm(list=ls())


library(ncdf4)
library(terra)
library(tidyverse)
library(sf)
library(readr)


# (1) Every FIeld ---------------------------------------------------------


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

# buffer 3 km
fields_buf <- st_buffer(fieldpolygons_proj, dist = 3000)
fields_buf_noforst <- fields_buf %>% filter(Name != "Forst Field")


rasters <- list(r2017,r2018,r2019, r2020, r2021, r2022,r2023)

# clip raster 
site_col <- "Name"   


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
  fname <- paste0("output/LULC/", gsub("[^a-zA-Z0-9]", "_", site_name),"_", year, "_buf3000m.tif")
  
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
  fname <- paste0("output/LULC/", gsub("[^a-zA-Z0-9]", "_", site_name),"_", year, "_buf3000m.tif")
  raster <- terra::rast(fname)
  levels(raster) <- lulc_legend
  plot(raster, main = paste0(site_name," ", year))
}
}


# (2) Forst ---------------------------------------------------------------

rm(list=ls())

# raster 
r2017 <- terra::rast("data/LULC/2017/33U_20170101-20180101.tif")
r2018 <- terra::rast("data/LULC/2018/33U_20180101-20190101.tif")
r2019 <- terra::rast("data/LULC/2019/33U_20190101-20200101.tif")
r2020 <- terra::rast("data/LULC/2020/33U_20200101-20210101.tif")
r2021 <- terra::rast("data/LULC/2021/33U_20210101-20220101.tif")
r2022 <- terra::rast("data/LULC/2022/33U_20220101-20230101.tif")
r2023 <- terra::rast("data/LULC/2023/33U_20230101-20240101.tif")

rasters <- list(r2017,r2018,r2019, r2020, r2021, r2022,r2023)

fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")
fieldpolygons_proj <- fieldpolygons %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(r2017))
fields_buf <- st_buffer(fieldpolygons_proj, dist = 3000)
fields_buf_forst <- fields_buf %>% filter(Name == "Forst Field")

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

site_col <- "Name"   

for (i in seq_len(nrow(fields_buf_forst))) {
  for (year_idx in seq_along(rasters)) {
    year <- 2016 + year_idx
    site_name <- fields_buf_forst[[site_col]][i]
    
    cat("Processing:", site_name, "\n")
    
    # convert sf polygon to SpatVector for terra
    poly <- vect(fields_buf_forst[i, ])
    
    # crop to bounding box first (fast), then mask to exact polygon shape
    levels(rasters[[year_idx]]) <- lulc_legend
    r_crop <- crop(rasters[[year_idx]], poly)
    r_mask <- mask(r_crop, poly)
    
    # clean filename — removes spaces and special characters
    fname <- paste0("output/LULC/", gsub("[^a-zA-Z0-9]", "_", site_name),"_", year, "_buf3000m.tif")
    
    writeRaster(r_mask, fname, overwrite = TRUE)
    cat("  Saved to:", fname, "\n")
    
  }
}

years <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023)

for (i in seq_len(nrow(fields_buf_forst))) {
  par(mfrow=c(2, 4))
  for (year in years){
    site_name <- fields_buf_forst[[site_col]][i]
    cat("Processing:", site_name, "\n")
    fname <- paste0("output/LULC/", gsub("[^a-zA-Z0-9]", "_", site_name),"_", year, "_buf3000m.tif")
    raster <- terra::rast(fname)
    levels(raster) <- lulc_legend
    plot(raster, main = paste0(site_name," ", year))
  }
}


# (3) Landscape metrics ---------------------------------------------------

rm(list=ls())

# landscape contagion (CONTAG) - configuration 
# lsm_l_contag(landscape, verbose = TRUE)

# number of patches (NP) - aggregation/framentation
#lsm_l_np

# simpson diversity index - composition
# lsm_si_di

# shannon diversity index - composition
#lsm_l_shdi

# aggregation index - aggregation
# lsm_l_ai

# edge density - configuration 
# lsm_l_ed 

r2017 <- terra::rast("data/LULC/2017/33U_20170101-20180101.tif")

fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")
fieldpolygons_proj <- fieldpolygons %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(r2017))
fields_buf <- st_buffer(fieldpolygons_proj, dist = 3000)

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

site_col <- "Name"

years <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023)

results <- data.frame(field = character(), year = numeric(),l_metric = character(), l_value = numeric())

for (i in seq_len(nrow(fields_buf))) {
  
  for (year in years){
    site_name <- fields_buf[[site_col]][i]
    cat("Processing:", site_name, year, "\n")
    fname <- paste0("output/LULC/", gsub("[^a-zA-Z0-9]", "_", site_name),"_", year, "_buf3000m.tif")
    raster <- terra::rast(fname)
    levels(raster) <- lulc_legend
    
    fieldresults <- data.frame(
      field  = site_name,
      year   = year,
      l_contag = lsm_l_contag(raster) |> dplyr::pull(value),
      l_np     = lsm_l_np(raster)     |> dplyr::pull(value),
      l_sidi   = lsm_l_sidi(raster)   |> dplyr::pull(value),
      l_shdi   = lsm_l_shdi(raster)   |> dplyr::pull(value),
      l_ai     = lsm_l_ai(raster)     |> dplyr::pull(value),
      l_ed     = lsm_l_ed(raster)     |> dplyr::pull(value)
    )
    
    results <- rbind(results, fieldresults)
  }
}

# 2016 has no values, so paste them from 2017

results_2016 <- results |>
  dplyr::filter(year == 2017) |>
  dplyr::mutate(year = 2016)

results <- dplyr::bind_rows(results, results_2016)

write.csv(results, "./data/AnalysisData/20260805_dflulc.csv", row.names = FALSE)


# (4) Merge with the main dataset  ----------------------------------------
rm(list=ls())
dfmain <- read.csv("./data/AnalysisData/20260805_AFswf.csv")
lulc <- read.csv("./data/AnalysisData/20260805_dflulc.csv")

levels(as.factor(dfmain$field))
levels(as.factor(lulc$field))


lulc <- lulc |> 
  mutate(id = case_when(
    field == "Ihinger Hof Field" ~ "IhingerHof",
    field == "Dornburg field" ~ "Dornburg",
    field == "Forst Field" ~ "Forst",
    field == "Gladbacherhof Field" ~ "Gladbacherhof",
    field == "Mariensee Field" ~ "Mariensee",
    field == "Reiffenhausen Field" ~ "Reiffenhausen",
    field == "Vechta Field" ~ "Vechta",
    field == "Wendhausen field" ~ "Wendhausen"
  ))


dflulc <- dfmain %>%
  left_join(
    lulc,
    by = c("field" = "id", "year"),
    relationship = "many-to-one"
  ) |> 
  select(-field.y)

write.csv(dflulc, "./data/AnalysisData/20260805_AFlulc.csv", row.names = FALSE)
