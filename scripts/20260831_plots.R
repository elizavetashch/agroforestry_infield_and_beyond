library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(ggrepel)
# Germany map
germany <- ne_countries(
  country = "Germany",
  scale = "medium",
  returnclass = "sf"
)

g <- df |> distinct(field, id, longitude, latitude)
# Aggregate observations by coordinates
sites <- g %>%
  count(field, longitude, latitude, name = "n")


# Plot
ggplot() +
  geom_sf(
    data = germany,
    fill = "white",
    color = "black",
    linewidth = 0.4
  ) +
  geom_point(
    data = sites,
    aes(longitude, latitude, size = n),
    shape = 21,
    fill = "#4D8B47",
    color = "black",
    alpha = 0.8
  ) +
  geom_text_repel(
    data = sites,
    aes(longitude, latitude, label = field),
    size = 3.5,
    box.padding = 0.5,
    point.padding = 0.3,
    max.overlaps = Inf
  ) +
  scale_size_continuous(name = "Observations") +
  coord_sf() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right"
  )
