

# Clean teh Environment
rm(list=ls())


# Packages ----------------------------------------------------------------

library(janitor)
library(dplyr)
library(tidyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(rnaturalearth)



# Koch 2025 ---------------------------------------------------------------

koch25 <- readr::read_delim("data/Koch25/yields_wintercrops.csv", delim = ";", locale = locale(decimal_mark = ","))
koch25$data_id <- "koch25"

koch25 <- janitor::clean_names(koch25)

names(koch25)
koch25$x1 <- NULL

nameskoch25 <- names(koch25)


# Paut 2023 ---------------------------------------------------------------

# not german 

# Baier 2023 --------------------------------------------------------------

# not german 


# Wendhausen  -------------------------------------------------------------

wendhausen_1518 <- read_csv("data/BONARES_Cropland agroforestry 2015-2018/SIGNAL.ID_7013_DATEN_WH_15_18.csv")
wendhausen_1518$data_id <- "wendhausen1518"

wendhausen_1718 <- read_csv("data/BONARES_Cropland Agroforestry 2017 and 2018/signal.ID_7042_BIOMASSE_17_18_WH_280319.csv")
wendhausen_1718$data_id <- "wendhausen1718"

wendhausen_1920 <- read_csv("data/BONARES_Cropland agroforestry 2019-2020/signal.ID_7069_DATEN_WENDHAUSEN_2019_20.csv")
wendhausen_1920$data_id <- "wendhausen1920"

wendhausen_21 <- read_csv("data/BONARES_Cropland agroforestry 2021/signal.ID_7070_DATEN_WENDHAUSEN_2021.csv")
wendhausen_21$data_id <- "wendhausen21"

wendhausen_22 <- read_csv("data/BONARES_Cropland agroforestry 2022/signal.ID_7071_DATEN_WENDHAUSEN_2022.csv")
wendhausen_22$data_id <- "wendhausen22"

wendhausen_23 <- read_csv("data/BONARES_Cropland agroforestry 2023/signal.ID_7077_DATEN_WENDHAUSEN_2023_CR.csv")
wendhausen_23$data_id <- "wendhausen23"


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

}))

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


# Hessen/Gladbacherhof -----------------------------------------------------------

hessen <- readxl::read_excel("data/ZALF_Hessen_2122/AFGH1_Yield_All.xlsx")
hessen$data_id <- "hessen"

hessen <- janitor::clean_names(hessen)

hessen <- hessen %>%
  mutate(
    date = as.Date(as.character(date)),
    year = format(date, "%Y"),
    year = as.numeric(year)
  )
# 
# hessen <- hessen %>% 
#   mutate(site = factor(site),
#          date = factor(date),
#          crop = factor(crop),
#          db_site_id = factor(db_site_id),
#          db_site_name = factor(db_site_name),
#          sample_name_db = factor(sample_name_db),
#          sample_name_field = factor(sample_name_field),
#          sample_ordering = factor(sample_ordering),
#          row = factor(row),
#          transect = factor(transect), 
#          direction = factor(direction))

gladbacherhof <- hessen %>% filter(site == "GH1")
gladbacherhof$data_id <- "gladbacherhof"




# Bremsberg4 ----------------------------------------------------------------------

bremsberg <- hessen %>% filter(site == "Bremsberg4")
bremsberg$data_id <- "bremsberg"

# Mariensee ---------------------------------------------------------------

mariensee1719 <- read_csv("data/Mariensee1719/signal.ID_7041_BIOMASSE_17_18_MS_280319.csv")
mariensee1719$data_id <- "mariensee1719"

mariensee1517 <- read_csv("data/Mariensee1517/signal.ID_7008_Gras_Laub_Holz_MS_2015_2016_2017.csv")
mariensee1517$data_id <- "mariensee1517"


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
dornburg16 <- read_csv("data/Bonares_Dornburg16/signal.ID_7004_PROD_D_2016_V2.csv")
dornburg16 <- clean_names(dornburg16)

str(dornburg16)
dornburg16$data_id <- "dornburg16"
dornburg16$year <- 2016

# Reiffenhausen -----------------------------------------------------------
reiffenhausen16 <- read_csv("data/BONARES_Reiffenhausen16/signal.ID_7039_REIFFENHAUSEN_BIOMASS_DATA_V2.csv")
reiffenhausen16 <- clean_names(reiffenhausen16)

str(reiffenhausen16)
# longitude and latitude are taken from the metadata
reiffenhausen16$lat <- 51.41
reiffenhausen16$long <- 9.98
reiffenhausen16$year <- 2016


reiffenhausen16$data_id <- "reiffenhausen16"

# Dornburg 18-23 ----------------------------------------------------------


dornburg1823 <- read_csv("data/BONARES_DornburgVechta1823/signal.ID_7088_CROP_YIELD.csv")
names(dornburg1823)



dornburg1823$site <- as.factor(dornburg1823$site)
levels(dornburg1823$site)

dornburg1823$data_id <- "dornburg1823"
dornburg1823 <- clean_names(dornburg1823)

# SIGNAL 2016 -------------------------------------------------------------

signal16 <- read_csv("data/BONARES_SIGNAL16/signal.ID_7048_BIOMASSES_SIGNAL_PROJECT_V1_APR_08_2020.csv")
names(signal16)
(signal16)

signal16$site <- as.factor(signal16$site)
levels(signal16$site)

signal16$data_id <- "signal16"
signal16 <- clean_names(signal16)


# Forst 2019-2020 ---------------------------------------------------------
forst1920 <- read_csv("data/BONARES_Forst1920/signal.ID_7060_CROP_YIELDS_FORST_2019_2020.csv")

names(forst1920)
str(forst1920)

forst1920 <- forst1920 %>%
  mutate(
    date = as.Date(as.character(date)),
    year = format(date, "%Y"),
    year = as.numeric(year)
  )

forst1920$data_id <- "forst1920"
forst1920 <- clean_names(forst1920)


# SIGNAL 18-23 : Dornburg1823 and Signal1823 are the same.------------------------------------------------------------


# Checked also the dornburg1823csv files, it is the same dataset. 

# signal1823 <- read_csv("data/BONARES_SIGNAL1823/signal.ID_7088_CROP_YIELD.csv")
# names(signal1823)
# 
# signal1823$site <- as.factor(signal1823$site)
# levels(signal1823$site)


# For all rename lat and long ---------------------------------------------

names(koch25) # already lat and long

(nameswendhausen <- names(wendhausen))
wendhausen <- rename(wendhausen, lat = latitude, long = longitude)

(nameshessen <- names(hessen))
hessen <- rename(hessen, lat = lat, long = lon)


(namesmariensee <- names(mariensee))
mariensee <- rename(mariensee, lat = latitude, long = longitude)

(namesdornburg16 <- names(dornburg16))

(namesdornburg1823 <- names(dornburg1823))

(namessignal16 <- names(signal16))

(namesforst1920 <- names(forst1920))
forst1920 <- rename(forst1920, lat = y, long = x)

# Plot all fields ---------------------------------------------------------

coords <- bind_rows(
  koch25       %>%  select(data_id, lat, long) %>% slice(1),
  wendhausen  %>% select(data_id, lat, long) %>% slice(1),
  gladbacherhof %>% select(data_id, lat, long) %>%slice(1),
  bremsberg   %>% select(data_id, lat, long) %>% slice(1),
  mariensee   %>% select(data_id, lat, long) %>% slice(1),
  dornburg    %>% select(data_id, lat, long) %>% slice(1),
  reiffenhausen %>% select(data_id, lat, long) %>% slice(1)
) %>% distinct()

coords

germany <- ne_countries(country = "Germany", returnclass = "sf")

coords_sf <- st_as_sf(
  coords,
  coords = c("long", "lat"),
  crs = 4326
)

ggplot() +
  geom_sf(data = germany, fill = "grey95", color = "black") +
  geom_sf(data = coords_sf, size = 3) +
  geom_sf_text(
    data = coords_sf,
    aes(label = data_id),
    nudge_x = 0.15,  # adjust position horizontally
    nudge_y = 0.2,  # adjust position vertically
    size = 3
  ) +  
  theme_minimal()


# Details on Datasets -----------------------------------------------------

dimkoch25 <- dim(koch25)
str(koch25)

# year (num)
# crop
# yield_wweight
# p_dist (is the distance from tree) 
hist(koch25$p_dist)
# design elements: id, block, treatment , aspect
# aspect: E, W 
koch25$aspect <- as.factor(koch25$aspect)
levels(koch25$aspect)

dimwend <- dim(wendhausen)
str(wendhausen)
# year (num)
# crop
# orientation: lee, luv 
# distance to tree strip


dimglad <- dim(gladbacherhof)
str(gladbacherhof)
# year (num) 
# crop 
# biomass_kg_m2, grain_kg_m2, total_kg_m2
# design: row, transect, direction, 
# distance ( is distance to tree) 

# details Koch 25 ----------------------------------------------------------

design_koch25 <- koch25 %>% 
  distinct(year, block, id, treatment, p_dist, crop, lat,long)

koch25 %>%
  count(block, id, treatment) %>%
  arrange(block, id)

design_koch25 %>%
  count(year, block, p_dist, treatment) %>%
  tidyr::pivot_wider(
    names_from = treatment,
    values_from = n,
    values_fill = 0
  )

# TREATMENT VISUALISATION:
ggplot(design_koch25,
       aes(long, lat,
           color = treatment,
           size = p_dist)) +
  geom_point(alpha = 0.8) +
  #facet_wrap(~year) +
  coord_equal() +
  theme_bw()


# CROP ROTATION:
ggplot(design_koch25,
       aes(long, lat,
           color = crop,
           size = p_dist)) +
  geom_point(alpha = 0.8) +
  facet_wrap(~year) +
  coord_equal() +
  theme_bw()




# Bind Rows ---------------------------------------------------------------


str(koch25)
koch25$id <- as.factor(koch25$id)

str(wendhausen)

str(hessen)

str(mariensee)

str(dornburg16)

str(dornburg1823)
dornburg1823$id <- as.factor(dornburg1823$id)

str(reiffenhausen16)
str(signal16)
signal16$id <- as.factor(signal16$id)

str(forst1920)
forst1920$id <- as.factor(forst1920$id)

df <- bind_rows(
  koch25,  wendhausen, hessen, mariensee, dornburg16, 
  dornburg1823, reiffenhausen16, signal16, forst1920) 



# Exploration of the new df dataset and reorganization of it --------------

names(df[order(names(df))])


# Year Merge  -------------------------------------------------------------

year_cols <- c(
"year",
"date",
"harvest_year"
)


df |>
  select(any_of(year_cols)) |>
  mutate(
    across(everything(), ~!is.na(.x)),
    .keep = "all"
  ) |>
  # Count unique presence patterns
  count(across(everything()), name = "n_rows") |>
  arrange(desc(n_rows))


# Which data points contain the date column -----------------------------------------------
datdf <- 
df %>% 
  filter(!is.na(date))
# its Hessen and Forst

df$year[is.na(df$year)] <- df$harvest_year[is.na(df$year)]

# Distance To Tree Merge --------------------

dist_cols <- c(
"dist",
"distance",
"p_dist",
"distance_from_tree_row",
"distance_to_tree_strip"
)


# For each row, show which of these columns has a non-NA value
df |>
  select(any_of(dist_cols)) |>
  mutate(
    across(everything(), ~!is.na(.x)),
    .keep = "all"
  ) |>
  # Count unique presence patterns
  count(across(everything()), name = "n_rows") |>
  arrange(desc(n_rows))

# ----> there are 1096 NA for distance to tree

## Merge the columns 

df <- df |>
  mutate(distance_to_tree = coalesce(as.character(dist), as.character(distance), as.character(p_dist), 
                                     as.character(distance_from_tree_row), as.character(distance_to_tree_strip))) |>
  select(-dist, -distance, -p_dist, -distance_from_tree_row, -distance_to_tree_strip)

# Quick check
sum(is.na(df$distance_to_tree)) # 1096 NA for distance to tree

write.csv(df, "data/AnalysisData/20260618_df.csv", row.names = FALSE)





