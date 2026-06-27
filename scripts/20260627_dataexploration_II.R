
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
library(paletteer)


# palette -----------------------------------------------------------------

farbenblind_light_contr9 <- c("#77AADD", "#99DDFF", "#44BB99", "#BBCC33","#AAAA00",
                              "#EEDD88", "#EE8866","#FFAABB", "#DDDDDD")
make_palette_graph(farbenblind_light_contr9)

# Koch 2025 ---------------------------------------------------------------

koch25 <- readr::read_delim("data/Koch25/yields_wintercrops.csv", delim = ";", locale = locale(decimal_mark = ","))
koch25$data_id <- "koch25"

koch25 <- janitor::clean_names(koch25)

(nameskoch25 <- names(koch25))

# (1) ACS Design  ---------------------------------------------------------

design_koch25 <- koch25  %>% 
  distinct(year, block, id, treatment, p_dist, crop, lat, long)

koch25  %>% 
  count(block, id, treatment)  %>% 
  arrange(block, id)

# BLOCK VISUALISATION
ggplot(design_koch25,
       aes(long, lat,
           color = as.factor(block))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = farbenblind_light_contr9) +
  theme_bw() +
  labs(
    title = "Ihinger Hof: Block Design",
    color = "Block Factor",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 5),
    nrow = 1           
  ))

# ID VISUALIZATION
ggplot(design_koch25,
       aes(long, lat, color = as.factor(id))) +
  geom_point(alpha = 0.8) +
  paletteer::scale_color_paletteer_d("ggsci::default_ucscgb") +
  theme_bw() +
  labs(
    title = "Ihinger Hof: ID Design",
    color = "ID",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(override.aes = list(size = 5),
                              nrow = 2)) 

levels(as.factor(koch25$id))

# (1) Investigate Yield Column  -------------------------------------------

koch25$yield_unit <- "t/ha" # known from the publicaiton

# 1.1. Record the unit of yield in the yield_unit column ------------------


# 1.2. yield distribution, check for outliers -----------------------------


# (2) Investigate Distance to Tree Row ------------------------------------


# (3) Invistigate and document Date and Year ------------------------------


# (4) Record crop in the crop column --------------------------------------


# (5) Record control data -------------------------------------------------


