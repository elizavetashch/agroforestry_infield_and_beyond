
# Clean teh Environment
rm(list=ls())

# Packages ----------------------------------------------------------------

pkgs <- c("ggplot2", "tidygraph", "dplyr", "tidyr", "tidyverse")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(readxl)
library(ggraph)
library(tidygraph)
library(dplyr)
library(tidyr)
library(tidyverse)
library(sf)
library(rnaturalearth)

# Read the Data -----------------------------------------------------------

df <- read_excel("data/Baier23/Effects of Agroforestry on Grain Yield of Maize_Main Dataset.xlsx")
df_orig <- df

df <- janitor::clean_names(df)

glimpse(df)
plot(df) # Attention! In can take a while if you have a big dataset



# Filter for Corine Countries if needed -----------------------------------


corinecountries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
                     "Czechia", "Denmark", "Estonia", "Finland", "France", 
                     "Germany", "Greece", "Hungary", "Ireland", "Italy", 
                     "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", 
                     "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
                     "Spain", "Sweden", "Iceland", "Liechtenstein", "Norway", 
                     "Switzerland", "Albania", "Bosnia and Herzegovina", "Kosovo", 
                     "Montenegro", "North Macedonia", "Serbia", "Turkey")
df <- df %>% filter(Country %in% corinecountries)


# Fix one name mismatch between corinecountries and Natural Earth
corinecountries_ne <- ifelse(corinecountries == "Bosnia and Herzegovina",
                             "Bosnia and Herz.",
                             corinecountries)

# Get and union country polygons
europe_sf <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(name %in% corinecountries_ne) |>
  st_union()

# Spatially filter df
df_noNA <- df |> filter(!is.na(latitude_decimal), !is.na(longitude_decimal))
df_sf   <- st_as_sf(df_noNA, coords = c("longitude_decimal", "latitude_decimal"), crs = 4326)

df_europe <- df_noNA |> filter(st_intersects(df_sf, europe_sf, sparse = FALSE)[, 1])

summary(df_europe)

# Note:  ------------------------------------------------------------------
 
# the dataset has only Turkey, Belgium, and Germany. 
# the German data point is Wendhasuen, which is already included in teh BONARES 

# -------------------------------------------------------------------------

df <- df_europe[1:3,]
glimpse(df)

# Final Step: 

write.csv(df, "data/collection/20260614_baier23.csv", row.names = FALSE)
