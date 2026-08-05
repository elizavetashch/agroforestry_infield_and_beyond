rm(list=ls())

library(dplyr)
library(tidyr)

# the slope raster was created in ArcGIS by taking the dtm from the data dtm folder
# and calculating the slope with the Slope tool in degrees by cutting it to the field polygon. 
# then used zonal statistics as a table to produce the csv table used in this calculation.

df <- read.csv("./data/AnalysisData/20260805_AFlulc.csv")
slope <- read.csv("./data/dtm/20260805_slope_fields.csv")

slope <- slope |> 
  mutate(id = case_when(
    Name == "Ihinger Hof Field" ~ "IhingerHof",
    Name == "Dornburg field" ~ "Dornburg",
    Name == "Forst Field" ~ "Forst",
    Name == "Gladbacherhof Field" ~ "Gladbacherhof",
    Name == "Mariensee Field" ~ "Mariensee",
    Name == "Reiffenhausen Field" ~ "Reiffenhausen",
    Name == "Vechta Field" ~ "Vechta",
    Name == "Wendhausen field" ~ "Wendhausen"
  )) |> 
  mutate(min_slope = MIN, 
         max_slope = MAX, 
         mean_slope = MEAN) |> 
  select(id, min_slope, max_slope, mean_slope)

dfslope <- df %>%
  left_join(
    slope,
    by = c("field" = "id"),
    relationship = "many-to-one"
  )

write.csv(dfslope, "./data/AnalysisData/20260805_AFslope.csv", row.names = FALSE)
write.csv(dfslope, "./analysis_data/20260805_AFslope.csv", row.names = FALSE)
