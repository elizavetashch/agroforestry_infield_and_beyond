
# Clean teh Environment
rm(list=ls())


df <-  read.csv("data/AnalysisData/20260618_df.csv")

names(df)


# Some rows dont have a coordinate ----------------------------------------
latna <- df[is.na(df$lat), ]



# Assign coordinates to vechta  -------------------------------------------

df <- df %>%
  mutate(
    lat = case_when(
      is.na(lat) & site == "Vechta" ~ 52.69159,
      TRUE ~ lat
    ),
    long = case_when(
      is.na(long) & site == "Vechta" ~ 8.24977,
      TRUE ~ long
    )
  )

# Year Distribution -------------------------------------------------------

hist(df$year)



# Spatial Distribution ----------------------------------------------------


germany <- ne_countries(country = "Germany", returnclass = "sf")

coords_sf <- sf::st_as_sf(
  df,
  df = c("long", "lat"),
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
