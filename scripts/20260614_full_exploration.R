

# Clean teh Environment
rm(list=ls())


# Packages ----------------------------------------------------------------

library(janitor)

# Koch 2025 ---------------------------------------------------------------

koch25 <- readr::read_delim("data/Koch25/yields_wintercrops.csv", delim = ";", locale = locale(decimal_mark = ","))
koch25 <- clean_names(koch25)
# Paut 2023 ---------------------------------------------------------------

# not german 

# Baier 2023 --------------------------------------------------------------

# not german 


# Wendhausen  -------------------------------------------------------------

wendhausen_1518 <- read_csv("data/BONARES_Cropland agroforestry 2015-2018/SIGNAL.ID_7013_DATEN_WH_15_18.csv")
wendhausen_1718 <- read_csv("data/BONARES_Cropland Agroforestry 2017 and 2018/signal.ID_7042_BIOMASSE_17_18_WH_280319.csv")
wendhausen_1920 <- read_csv("data/BONARES_Cropland agroforestry 2019-2020/signal.ID_7069_DATEN_WENDHAUSEN_2019_20.csv")
wendhausen_21 <- read_csv("data/BONARES_Cropland agroforestry 2021/signal.ID_7070_DATEN_WENDHAUSEN_2021.csv")
wendhausen_22 <- read_csv("data/BONARES_Cropland agroforestry 2022/signal.ID_7071_DATEN_WENDHAUSEN_2022.csv")
wendhausen_23 <- read_csv("data/BONARES_Cropland agroforestry 2023/signal.ID_7077_DATEN_WENDHAUSEN_2023_CR.csv")

wendhausen <- list(
  wendhausen_1518 = clean_names(wendhausen_1518), 
  wendhausen_1718 = clean_names(wendhausen_1718), 
  wendhausen_1920 = clean_names(wendhausen_1920), 
  wendhausen_21 = clean_names(wendhausen_21), 
  wendhausen_22 = clean_names(wendhausen_22), 
  wendhausen_23 = clean_names(wendhausen_23))


# Wendhausen: Standardise and merge all wendhausen datasets --------------
 
merged <- bind_rows(lapply(names(wendhausen), function(ds) {
  df <- wendhausen[[ds]]
  
  # Lowercase all column names
  names(df) <- tolower(names(df))
  
  # Merge lat/latitude → latitude, lon/longitude → longitude
  if ("lat" %in% names(df) && !"latitude" %in% names(df))
    df <- rename(df, latitude = lat)
  if ("lon" %in% names(df) && !"longitude" %in% names(df))
    df <- rename(df, longitude = lon)
  
  
  # Merge ww and wheat 
  ww_cols <- grep("^ww_", names(df), value = TRUE)
  for (col in ww_cols) {
    wheat_col <- sub("^ww_", "wheat_", col)
    if (!wheat_col %in% names(df))
      df <- rename(df, !!wheat_col := !!col)
  }
  
  
  # Drop unwanted columns
  df <- select(df, -any_of(c("decomposition", "litter_dm", "objectid", "lat", "lon")))
  
  # Add origin column
  df$origin <- ds
  df
}))

# Move origin to first column
merged <- select(merged, origin, everything())


merged_long <- merged %>%
  pivot_longer(
    cols = matches("^(or|wheat|straw|sm|sb)_"),
    names_to  = c("crop", ".value"),
    names_pattern = "^(or|wheat|straw|sm|sb)_(.*)",
    names_transform = list(crop = as.factor)
  )


# -------------------------------------------------------------------------

wendhausen <- merged_long
rm(merged, merged_long, wendhausen_1518, wendhausen_1718, wendhausen_1920, wendhausen_21, wendhausen_22, wendhausen_23)


# Gladbacherhof -----------------------------------------------------------

hessen <- readxl::read_excel("data/ZALF_Hessen_2122/AFGH1_Yield_All.xlsx")

hessen <- janitor::clean_names(hessen)


hessen <- hessen %>% 
  mutate(site = factor(site),
         date = factor(date),
         crop = factor(crop),
         db_site_id = factor(db_site_id),
         db_site_name = factor(db_site_name),
         sample_name_db = factor(sample_name_db),
         sample_name_field = factor(sample_name_field),
         sample_ordering = factor(sample_ordering),
         row = factor(row),
         transect = factor(transect), 
         direction = factor(direction))

gladbacherhof <- hessen %>% filter(site == "GH1")




# Bremsberg4 ----------------------------------------------------------------------

bremsberg <- hessen %>% filter(site == "Bremsberg4")


# Mariensee ---------------------------------------------------------------

mariensee1719 <- read_csv("data/Mariensee1719/signal.ID_7041_BIOMASSE_17_18_MS_280319.csv")
mariensee1517 <- read_csv("data/Mariensee1517/signal.ID_7008_Gras_Laub_Holz_MS_2015_2016_2017.csv")

mariensee1719 <- clean_names(mariensee1719)
mariensee1517 <- clean_names(mariensee1517)

mariensee <- bind_rows(
  mariensee1719 |>
    select(-objectid),
  mariensee1517 |>
    mutate(year = as.numeric(format(date, "%Y"))) |>
    rename(latitude = lat, longitude = lon) |>
    select(-objectid, -date)
)
glimpse(mariensee)


mariensee  %>%  count(year)

m17 <- mariensee  %>% 
  filter(year == 2017) 
  
# Dornburg ----------------------------------------------------------------
dornburg <- read_csv("data/Bonares_Dornburg/signal.ID_7004_PROD_D_2016_V2.csv")
dornburg <- clean_names(dornburg)

str(dornburg)

# Reiffenhausen -----------------------------------------------------------
reiffenhausen <- read_csv("data/BONARES_Reiffenhausen/signal.ID_7039_REIFFENHAUSEN_BIOMASS_DATA_V2.csv")
reiffenhausen <- clean_names(reiffenhausen)

str(reiffenhausen)
# longitude and latitude are taken from the metadata
reiffenhausen$lat <- 51.41
reiffenhausen$long <- 9.98


# For all rename lat and long ---------------------------------------------

str(koch25) # already lat and long

str(wendhausen)
wendhausen <- rename(wendhausen, lat = latitude, long = longitude)

str(gladbacherhof)
gladbacherhof <- rename(gladbacherhof, long = lon)

str(bremsberg)
bremsberg <- rename(bremsberg, long = lon)

str(mariensee)
mariensee <- rename(mariensee, lat = latitude, long = longitude)

str(dornburg)

str(reiffenhausen)

# Plot all fields ---------------------------------------------------------

coords <- bind_rows(
  koch25      |> select(lat, long) |> slice(1),
  wendhausen  |> select(lat, long) |> slice(1),
  gladbacherhof |> select(lat, long) |> slice(1),
  bremsberg   |> select(lat, long) |> slice(1),
  mariensee   |> select(lat, long) |> slice(1),
  dornburg    |> select(lat, long) |> slice(1),
  reiffenhausen |> select(lat, long) |> slice(1)
) |> distinct()

coords


library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)

germany <- ne_countries(country = "Germany", returnclass = "sf")

coords_sf <- st_as_sf(
  coords,
  coords = c("long", "lat"),
  crs = 4326
)

ggplot() +
  geom_sf(data = germany, fill = "grey95", color = "black") +
  geom_sf(data = coords_sf, size = 3) +
  coord_sf() +
  theme_minimal()