


library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(rnaturalearth)
library(terra)
library(stringr)

df <- read_excel("C:/Users/lisa7/Desktop/Master Studium/Masterarbeit_Local/01_InputData/Effects of Agroforestry on Grain Yield of Maize_Main Dataset.xlsx")

# origin study: 
# https://www.frontiersin.org/journals/sustainable-food-systems/articles/10.3389/fsufs.2023.1167686/full

summary(df)

glimpse(df)



points_sf <- st_as_sf(df, coords = c("Longitude Decimal", "Latitude Decimal"), crs = 4326)

world <- ne_countries(scale = "medium", returnclass = "sf")
europe <- world %>% filter(continent == "Europe")

points_europe <- st_join(points_sf, europe, join = st_within) %>%
  filter(!is.na(continent))

germany <- world %>% filter(admin == "Germany")

points_germany <- st_join(points_sf, germany, join = st_within) %>%
  filter(!is.na(admin))




# Germany (rough box)
df_germany <- df %>%
  filter(lat >= 47, lat <= 55,
         lon >= 5, lon <= 16)

df1 <- df %>%
  rename(Latitude_Decimal = `Latitude Decimal`)

df1 <- df1 %>%
  rename(Longitude_Decimal = `Longitude Decimal`)

# Europe (very rough!)
df_europe <- df1 %>%
  filter(Latitude_Decimal >= 35, Latitude_Decimal <= 72,
         Longitude_Decimal >= -25, Longitude_Decimal <= 45)


##### 
koppen <- rast("..\\01_InputData\\kg\\koppen_geiger_0p1.tif")
plot(koppen)


df1 <- df1 %>% drop_na(Longitude_Decimal)
points_sf <- st_as_sf(df1, coords = c("Longitude_Decimal", "Latitude_Decimal"), crs = 4326)
points_vect <- vect(points_sf)
df1$climate <- terra::extract(koppen, points_vect)[,2]

koppen_lookup <- c(
  "1"  = "Af",
  "2"  = "Am",
  "3"  = "Aw",
  "4"  = "BWh",
  "5"  = "BWk",
  "6"  = "BSh",
  "7"  = "BSk",
  "8"  = "Csa",
  "9"  = "Csb",
  "10" = "Csc",
  "11" = "Cwa",
  "12" = "Cwb",
  "13" = "Cwc",
  "14" = "Cfa",
  "15" = "Cfb",
  "16" = "Cfc",
  "17" = "Dsa",
  "18" = "Dsb",
  "19" = "Dsc",
  "20" = "Dsd",
  "21" = "Dwa",
  "22" = "Dwb",
  "23" = "Dwc",
  "24" = "Dwd",
  "25" = "Dfa",
  "26" = "Dfb",
  "27" = "Dfc",
  "28" = "Dfd",
  "29" = "ET",
  "30" = "EF"
)

passt <- c(8:16)
passt <- as.character(passt)
df1$climate <- koppen_lookup[as.character(res[,2])]
df_temperate <- df1 %>%
  filter(climate == passt)

df_temperate