
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
library(stringr)


library(tidyverse)   # data wrangling + ggplot2
library(lattice)     # dotplots, xyplots (Zuur's preferred tool)
library(car)         # vif(), scatterplotMatrix()
library(lme4)        # for Step 8 autocorrelation check via random effects
library(performance) # check_normality(), check_collinearity()


# palette -----------------------------------------------------------------

farbenblind_light_contr9 <- c("#77AADD", "#99DDFF", "#44BB99", "#BBCC33","#AAAA00",
                              "#EEDD88", "#EE8866","#FFAABB", "#DDDDDD")

# =============================================================================
# Data structure: Koch 25
#   field: Ihinger Hof 
#   ACS Design  — block, id
#   year        — repeated sampling across time
#   p_dist      — distance to measurement point (blocking factor)
#   yield_wweight — response variable
#   tree - hedge, willow, walnut (treatment column)
# =============================================================================

koch25 <- readr::read_delim("data/Koch25/yields_wintercrops.csv", delim = ";", locale = locale(decimal_mark = ","))
koch25$data_id <- "koch25"
koch25$field <- "IhingerHof"

koch25 <- janitor::clean_names(koch25)

(nameskoch25 <- names(koch25))

# (1) ACS Design  ---------------------------------------------------------

# 1.1. Year, Block, Id design ---------------------------------------------

koch25 %>% dplyr::summarise(
  n_ids  = dplyr::n_distinct(id),
  n_blocks = dplyr::n_distinct(block),
  n_treatments = dplyr::n_distinct(treatment),
  n_obs  = dplyr::n_distinct(x1),
  .by = year
)

koch25 %>% dplyr::summarise(
  n_obs  = dplyr::n_distinct(x1),
  .by = c(year, block)
)

# 1.2. Design Visualization -----------------------------------------------


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

koch25$x1 <- NULL
koch25$yield_unit <- "t/ha" # known from the publicaiton
names(koch25)[names(koch25) == "treatment"] <- "tree_species"

names(koch25)[names(koch25) == "p_dist"] <- "distance_to_tree_row"
names(koch25)[names(koch25) == "yield_wweight"] <- "yield"

names(koch25)

# check:
colSums(is.na(koch25)) # 0
nrow(koch25) # 28174


write.csv(koch25, file = "data/AnalysisData/20260629_ihingerhof.csv", row.names = FALSE)

# =============================================================================
# Data structure: SIGNAL 16
#   field: Wendhausen, Dornburg, Mariensee, Forst 
#   ACS Design  — plot (r1:r4) 20 forestry and 4 control sites 
#   year        — 2016, 2017
#   distance to tree row     — "distance_to_tree_strip"
#   yield — "grain_corn_or_grass_dry_mass_16" 
#            "ww_crude_protein"       "decomposition"          "litter_dm"              "wood_yield_estimated"
#   coordinates - lat and lon
# =============================================================================

signal16 <- read_csv("data/BONARES_SIGNAL16/signal.ID_7048_BIOMASSES_SIGNAL_PROJECT_V1_APR_08_2020.csv")

signal16$data_id <- "signal16"
signal16 <- janitor::clean_names(signal16)
names(signal16)


wendhausen_1920 <- wendhausen_1920[, -(10:15)]
wendhausen_1920$crop = "silage maize"
names(wendhausen_1920)[names(wendhausen_1920) == "grain_corn_or_grass_dry_mass_16"] <- "yield"
names(wendhausen_1920)[names(wendhausen_1920) == "straw_or_grass_dry_mass_2016"] <- "yield_straw"

# some rows have na in yield, because there only wood was harvested -> remove
wendhausen_1920 <- wendhausen_1920[!(is.na(wendhausen_1920$yield) & is.na(wendhausen_1920$distance_to_tree_strip)), ]

# check:
colSums(is.na(wendhausen_1920)) # 8 in distance those are controls
nrow(wendhausen_1920) # 48



# =============================================================================
# Data structure: Wendhausen
#   field: Wendhausen
#   ACS Design  — plot (r1:r4) 20 forestry and 4 control sites 
#   year        — year
#   distance to tree row     — "distance_to_tree_strip"
#   yield — "or_dm"    "or_dm_content"  "or_1000seed" "or_crude_fat" "ww_dm" "ww_dm_content" "ww_1000seed" 
#            "ww_crude_protein"       "decomposition"          "litter_dm"              "wood_yield_estimated"
#   coordinates - lat and lon
# =============================================================================

# 
# # Wendhausen 15 18 --------------------------------------------------------
# 
# 
# wendhausen_1518 <- read_csv("data/BONARES_Cropland agroforestry 2015-2018/SIGNAL.ID_7013_DATEN_WH_15_18.csv")
# wendhausen_1518$data_id <- "wendhausen1518"
# 
# # NAMES
# (names(wendhausen_1518) <- names(wendhausen_1518))
# wendhausen_1518 <- janitor::clean_names(wendhausen_1518)
# (names(wendhausen_1518) <- names(wendhausen_1518))
# 
# # NA check:
# nacheck <- wendhausen_1518 %>%
#   filter(is.na(or_dm) & is.na(ww_dm)) %>% 
#   filter(year != 2015) %>% 
#   select(year, plot, distance_to_tree_strip)
# 
# # Decision: filter out the observations, that have NA in distance to tree, and are NAs in yield 
# nrow(wendhausen_1518) # 84 
# wendhausen_1518 <- wendhausen_1518 %>%
#   filter(year != 2015) %>%
#   filter(!(is.na(or_dm) & is.na(ww_dm) & is.na(distance_to_tree_strip)))
# 
# # check
# nrow(wendhausen_1518) # 48
# colSums(is.na(wendhausen_1518)) # 8 observations na in distance - those are controls 
# 
# wendhausen_1518 <- wendhausen_1518[wendhausen_1518$year == 2016, ]
# 
# # Assign CROP 
# wendhausen_1518 <- wendhausen_1518 %>%
#   mutate(
#     crop = case_when(
#       year == 2016 ~ "oil rape",
#       year == 2017 ~ "winter wheat",
#       TRUE ~ NA_character_
#     ),
#     yield = coalesce(or_dm, ww_dm)
#   ) %>%
#   select(-or_dm, -ww_dm) %>% 
#   select(lat:soil_type, plot:yield)
# 
# 
# # check:
# colSums(is.na(wendhausen_1518)) # 8 in distance 
# nrow(wendhausen_1518) # 48 
# 
# 
# 
# # Wendhausen 17 18 --------------------------------------------------------
# 
# wendhausen_1718 <- read_csv("data/BONARES_Cropland Agroforestry 2017 and 2018/signal.ID_7042_BIOMASSE_17_18_WH_280319.csv")
# wendhausen_1718$data_id <- "wendhausen1718"
# (names(wendhausen_1718) <- names(wendhausen_1718))
# 
# wendhausen_1718 <- janitor::clean_names(wendhausen_1718)
# 
# wendhausen_1718 <- wendhausen_1718[, -c(1, 12:15)]
# wendhausen_1718$crop = "winter wheat"
# names(wendhausen_1718)[names(wendhausen_1718) == "wheat_dm"] <- "yield"
# names(wendhausen_1718)[names(wendhausen_1718) == "straw_dm"] <- "yield_straw"
# names(wendhausen_1718)[names(wendhausen_1718) == "longitude"] <- "lon"
# names(wendhausen_1718)[names(wendhausen_1718) == "latitude"] <- "lat"
# 
# # some rows have na in yield, because there only wood was harvested -> remove
# wendhausen_1718 <- wendhausen_1718[!(is.na(wendhausen_1718$yield_straw) & is.na(wendhausen_1718$distance_to_tree_strip)), ]
# 
# # check:
# colSums(is.na(wendhausen_1718)) # 8 in distance those are controls
# nrow(wendhausen_1718) # 48 
# 
# wendhausen_x1 <- bind_rows(wendhausen_1518,wendhausen_1718)
# 
# # check:
# colSums(is.na(wendhausen_x1)) # 16 in distance those are controls
# nrow(wendhausen_x1) # 96

# Wendhausen 19 20 --------------------------------------------------------

wendhausen_1920 <- read_csv("data/BONARES_Cropland agroforestry 2019-2020/signal.ID_7069_DATEN_WENDHAUSEN_2019_20.csv")
wendhausen_1920$data_id <- "wendhausen1920"
wendhausen_1920 <- janitor::clean_names(wendhausen_1920)
names(wendhausen_1920)


wendhausen_1920 <- wendhausen_1920[, -(10:15)]
wendhausen_1920$crop = "silage maize"
names(wendhausen_1920)[names(wendhausen_1920) == "sm_dm"] <- "yield"

# some rows have na in yield, because there only wood was harvested -> remove
wendhausen_1920 <- wendhausen_1920[!(is.na(wendhausen_1920$yield) & is.na(wendhausen_1920$distance_to_tree_strip)), ]

# check:
colSums(is.na(wendhausen_1920)) # 8 in distance those are controls
nrow(wendhausen_1920) # 48


wendhausen_x2 <- bind_rows(wendhausen_x1,wendhausen_1920)

# check:
colSums(is.na(wendhausen_x2)) # 24 in distance those are controls
nrow(wendhausen_x2) # 144

# Wendhausen 21 --------------------------------------------------------

wendhausen_21 <- read_csv("data/BONARES_Cropland agroforestry 2021/signal.ID_7070_DATEN_WENDHAUSEN_2021.csv")
wendhausen_21$data_id <- "wendhausen21"
wendhausen_21 <- janitor::clean_names(wendhausen_21)
names(wendhausen_21)

names(wendhausen_21)[names(wendhausen_21) == "sb_dm_straw"] <- "yield_straw"
names(wendhausen_21)[names(wendhausen_21) == "sb_dm"] <- "yield"
wendhausen_21$crop <- "summer barley"

names(wendhausen_21)
wendhausen_21 <- wendhausen_21[, -(11:15)]

# some rows have na in yield, because there only wood was harvested -> remove
wendhausen_21 <- wendhausen_21[!(is.na(wendhausen_21$yield) & is.na(wendhausen_21$distance_to_tree_strip)), ]

# check:
colSums(is.na(wendhausen_21)) # 4 in distance those are controls
nrow(wendhausen_21) # 24

wendhausen_x3 <- bind_rows(wendhausen_x2,wendhausen_21)

# check:
colSums(is.na(wendhausen_x3)) #
nrow(wendhausen_x3) # 168


# Wendhausen 22 --------------------------------------------------------

wendhausen_22 <- read_csv("data/BONARES_Cropland agroforestry 2022/signal.ID_7071_DATEN_WENDHAUSEN_2022.csv")
wendhausen_22$data_id <- "wendhausen22"
wendhausen_22 <- janitor::clean_names(wendhausen_22)
names(wendhausen_22)

wendhausen_22 <- wendhausen_22[, -(10:14)]
wendhausen_22$crop = "oil rape"
names(wendhausen_22)[names(wendhausen_22) == "or_dm"] <- "yield"

# some rows have na in yield, because there only wood was harvested -> remove
wendhausen_22 <- wendhausen_22[!(is.na(wendhausen_22$yield) & is.na(wendhausen_22$distance_to_tree_strip)), ]

# check:
colSums(is.na(wendhausen_22)) # 4, since only one year 
nrow(wendhausen_22) # 24, since only one year 

wendhausen_x4 <- bind_rows(wendhausen_x3,wendhausen_22)

# check:
colSums(is.na(wendhausen_x4)) # 36 
nrow(wendhausen_x4) # 192

# Wendhausen 23 --------------------------------------------------------

wendhausen_23 <- read_csv("data/BONARES_Cropland agroforestry 2023/signal.ID_7077_DATEN_WENDHAUSEN_2023_CR.csv")
wendhausen_23$data_id <- "wendhausen23"

wendhausen_23 <- janitor::clean_names(wendhausen_23)
names(wendhausen_23)

wendhausen_23 <- wendhausen_23[, -(10:14)]
wendhausen_23$crop = "sillage maize"
names(wendhausen_23)[names(wendhausen_23) == "sm_dm"] <- "yield"

# some rows have na in yield, because there only wood was harvested -> remove
wendhausen_23 <- wendhausen_23[!(is.na(wendhausen_23$yield) & is.na(wendhausen_23$distance_to_tree_strip)), ]

# check:
colSums(is.na(wendhausen_23)) # 4, since only one year 
nrow(wendhausen_23) # 24, since only one year 

wendhausen_x5 <- bind_rows(wendhausen_x4,wendhausen_23)

# check:
colSums(is.na(wendhausen_x5)) # 36
nrow(wendhausen_x5) # 216


# Wendhausen 2016-2023 ----------------------------------------------------

# ORGANIZE AND RENAME
names(wendhausen_x5)[names(wendhausen_x5) == "distance_to_tree_strip"] <- "distance_to_tree_row"
names(wendhausen_x5)[names(wendhausen_x5) == "lon"] <- "long"
wendhausen_x5$tree_species <- "poplar"
wendhausen_x5$yield_unit <- "t/ha"
wendhausen_x5$site <- NULL
names(wendhausen_x5)

# check 
table(wendhausen_x5$year) # in 2017 and 2021 there are 48 observations due to straw, the rest 24

# field assignment
wendhausen_x5$field <- "Wendhausen"

write.csv(wendhausen_x5, file = "data/AnalysisData/20260630_wendhausen.csv", row.names = FALSE)


# =============================================================================
# Data structure: Mariensee
#   field: Wendhausen
#   ACS Design  — plot, (r1:r6) 30 forestry and 6 control sites  
#   year        — year
#   distance to tree row     — "distance_to_tree_strip"
#   yield — grass_dm
#   coordinates - latitude longitude
# =============================================================================

# Mariensee 17 19 --------------------------------------------------------

mariensee1719 <- read_csv("data/Mariensee1719/signal.ID_7041_BIOMASSE_17_18_MS_280319.csv")
mariensee1719$data_id <- "mariensee1719"

mariensee1719 <- janitor::clean_names(mariensee1719)
names(mariensee1719)

mariensee1719 <- mariensee1719[, c(3:10, 15)]
mariensee1719$crop = "grass"
names(mariensee1719)[names(mariensee1719) == "grass_dm"] <- "yield"


names(mariensee1719)[names(mariensee1719) == "distance_to_tree_strip"] <- "distance_to_tree_row"
names(mariensee1719)[names(mariensee1719) == "longitude"] <- "long"
names(mariensee1719)[names(mariensee1719) == "latitude"] <- "lat"
mariensee1719$tree_species <- "willow"
mariensee1719$yield_unit <- "t/ha"
names(mariensee1719)

# check:
colSums(is.na(mariensee1719)) # 24
nrow(mariensee1719) # 84


# some rows have na in yield, because there only wood was harvested -> remove
mariensee1719 <- mariensee1719[!(is.na(mariensee1719$yield) & is.na(mariensee1719$distance_to_tree_row)), ]

# check:
colSums(is.na(mariensee1719)) # 12: 2 years with 6 controls 
nrow(mariensee1719) # 72: 2 years with 6 plots, 5 distances 4 times 

# check:
mariensee1719[mariensee1719$year == 2017, ] # 36
mariensee1719[mariensee1719$year == 2018, ] # 36

names(mariensee1719)

# Mariensee 15 17 --------------------------------------------------------
# 
# mariensee1517 <- read_csv("data/Mariensee1517/signal.ID_7008_Gras_Laub_Holz_MS_2015_2016_2017.csv")
# mariensee1517$data_id <- "mariensee1517"
# 
# mariensee1517 <- janitor::clean_names(mariensee1517)
# names(mariensee1517)
# 
# mariensee1517 <- mariensee1517[, c(3:10, 20)]
# mariensee1517$crop = "grass"
# names(mariensee1517)[names(mariensee1517) == "grass_dm"] <- "yield"
# 
# 
# names(mariensee1517)[names(mariensee1517) == "distance_to_tree_strip"] <- "distance_to_tree_row"
# names(mariensee1517)[names(mariensee1517) == "longitude"] <- "long"
# names(mariensee1517)[names(mariensee1517) == "latitude"] <- "lat"
# mariensee1517$tree_species <- "willow"
# mariensee1517$yield_unit <- "t/ha"
# names(mariensee1517)
# 
# # check:
# colSums(is.na(mariensee1517)) # 12/24/48 - different
# nrow(mariensee1517) # 108
# 
# 
# # some rows have na in yield, because there only wood was harvested -> remove
# mariensee1517 <- mariensee1517[!(is.na(mariensee1517$yield) & is.na(mariensee1517$distance_to_tree_row)), ]
# 
# # check:
# colSums(is.na(mariensee1517)) # 12: 1 year with 6 controls , but still 36 yields empty
# nrow(mariensee1517) # 96
# 
# # After investigating the dataset decided to only use year 2016 since
# # 2017 is present in the orevious dataset, and 2015 has only wood yield 
# 
# # filter:
# mariensee1517 <- mariensee1517[mariensee1517$date == as.Date("2016-01-01"), ]# 36
# 
# # rename plots and create year column
#   plot_map <- c(
#     "1" = "M-AF-r1",
#     "2" = "M-AF-r2",
#     "3" = "M-AF-r3",
#     "4" = "M-AF-r4",
#     "5" = "M-AF-r5",
#     "6" = "M-AF-r6",
#     "R1" = "M-C-r1",
#     "R2" = "M-C-r2",
#     "R3" = "M-C-r3",
#     "R4" = "M-C-r4",
#     "R5" = "M-C-r5",
#     "R6" = "M-C-r6"
#   )
# 
# mariensee1517 <- mariensee1517 %>%
#   mutate(
#     plot = plot_map[plot],
#     year = as.integer(format(date, "%Y"))
#   ) %>%
#   select(-date) %>%
#   rename(long = lon)
# 
# # bind rows 
# mariensee <- bind_rows(mariensee1517,mariensee1719)
# 
# # check:
# colSums(is.na(mariensee)) # 12/18 - different
# nrow(mariensee) # 108
# 
# mariensee$distance_to_tree_row[grepl("C", mariensee$plot)] <- NA
# 
# # check:
# colSums(is.na(mariensee)) # 18: 3 years 3 controls 6 plots -> 18 NA
# nrow(mariensee) # 108: 36 plots per year 3 years 
# 
# # check 
# table(mariensee$year) # 36 per year
# 
# # field assignment
# mariensee$field <- "Mariensee"
# 
# write.csv(mariensee, file = "data/AnalysisData/20260630_mariensee.csv", row.names = FALSE)


# =============================================================================
# Data structure: Dornburg
#   field: Dornburg
#   ACS Design  — plot, (r1:r6) 30 forestry and 6 control sites  
#   year        — year
#   distance to tree row     — "distance_to_tree_strip"
#   yield — grass_dm
#   coordinates - latitude longitude
# =============================================================================


# Dornburg 18 23 ----------------------------------------------------------------

dornburg1823 <- read_csv("data/BONARES_DornburgVechta1823/signal.ID_7088_CROP_YIELD.csv")
dornburgvechta <- dornburg1823

dornburg1823 <- janitor::clean_names(dornburg1823)
names(dornburg1823)
head(dornburg1823)

dornburg1823 <- dornburg1823 %>%
  mutate(distance_to_tree_row = as.numeric(str_extract(id, "\\d+(?=m)")))


dornburg1823$data_id <- "dornburg1823"
dornburg1823$tree_species <- "poplar"
dornburg1823$yield_unit <- "g/m2"

levels(as.factor(dornburg1823$site))

dornburg1823 <- dornburg1823 %>%
  mutate(
    fertilizer = str_extract(site, "normal|reduced")
  )

dornburg1823 <- dornburg1823 %>%
  mutate(
    field = case_when(
      str_detect(site, "Dornburg") ~ "Dornburg",
      site == "Vechta" ~ "Vechta",
      TRUE ~ NA_character_
    )
  )

# check:
colSums(is.na(dornburg1823)) # 2 in yield, 128 in distance to tree row 
nrow(dornburg1823) # 404

dornburg1823[(is.na(dornburg1823$grain_or_corn_dry_mass)), ] # let them be 
dornburg1823[(is.na(dornburg1823$distance_to_tree_row)), ] # its all control treatments 
names(dornburg1823)[names(dornburg1823) == "harvest_year"] <- "year"
names(dornburg1823)[names(dornburg1823) == "grain_or_corn_dry_mass"] <- "yield"
head(dornburg1823)

dornburg1823 <- dornburg1823[, c(1, 3:6, 8:14)]

# check 
table(dornburg1823$year) # 24 in 2018, 68 in 2019, 72 in 2020, 84 in 2021, 80 in 2022, 76 in 2023

dornburg1823 <- dornburg1823[dornburg1823$field == "Dornburg", ]

# check:
colSums(is.na(dornburg1823)) # 2 in yield, 128 in distance to tree row 
nrow(dornburg1823) # 284
table(dornburg1823$year) #  24 2018,   48 2019,   48 2020,  56 2021 ,  56 2022,  52 2023

# 2018: 4 distances, 4 plots, 4 controls
#       also D_AFcrop_r1 -> exlude those 
# 2019: same 24 but reduced and normal fertilization -> 48
# 2020: same
z <- dornburg1823[dornburg1823$year == 2021, ]
# 2021: in the Dornburg normal additional 18 m distance "D_AF_r1_18m", 
#       also D_AFcrop_r1 -> exlude those 

dornburg1823 <- dornburg1823[!grepl("AFcrop", dornburg1823$id), ]

# check
table(dornburg1823$year) #  20 2018,   40 2019,   40 2020,  48 2021 ,  48 2022,  44 2023

# 2023: sometimes 7 m is missing 

# bind rows

write.csv(dornburg1823, file = "data/AnalysisData/20260701_dornburg.csv", row.names = FALSE)

# Vechta ------------------------------------------------------------------

vechta <- dornburgvechta[dornburgvechta$field == "Vechta", ] # 120 
table(vechta$year) # 20 in 2019, 24 in 2020, 28 in 2021, 24 in 2022, 24 in 2023

# AFcrop is weighted by the area coverage of the sampling distances in AF
# exclude those 

nrow(vechta) # 120
vechta[grepl("AFcrop", vechta$id), ] # 20
vechta <- vechta[!grepl("AFcrop", vechta$id), ]
nrow(vechta) # 100

table(vechta$year) # 16 in 2019, 20 in 2020, 24 in 2021, 20 in 2022, 20 in 2023
# in 2019 onlu three distances - 1, 7 and 24 m 
# in 2021 very weird V_AF_r1_18m, 4 observations from the same r1_18m 
# leva them for now 

# check
head (vechta)
colSums(is.na(vechta)) # 20 in distance -> 5 years 4 controls 
nrow(vechta) # 100

write.csv(vechta, file = "data/AnalysisData/20260701_vechta.csv", row.names = FALSE)

# Forst 2019-2020 ---------------------------------------------------------
forst1920 <- read_csv("data/BONARES_Forst1920/signal.ID_7060_CROP_YIELDS_FORST_2019_2020.csv")

forst1920$data_id <- "forst1920"

forst1920 <- janitor::clean_names(forst1920)
names(forst1920)
head(forst1920)

forst1920 <- forst1920[, c(2:6, 14, 19:20)]

# latitude longitude
names(forst1920)[names(forst1920) == "x"] <- "long"
names(forst1920)[names(forst1920) == "y"] <- "lat"

# distance to tree row

# crop and yield 
names(forst1920)[names(forst1920) == "corn_dry_weight_t_per_ha"] <- "yield"
names(forst1920)[names(forst1920) == "straw_dry_weight_t_per_ha"] <- "yield_straw"


# date to year 
forst1920 <- forst1920 %>%
  mutate(
    year = as.integer(format(date, "%Y"))
  ) %>%
  select(-date) 


mariensee1517 <- mariensee1517[, c(3:10, 20)]
mariensee1517$crop = "grass"
names(mariensee1517)[names(mariensee1517) == "grass_dm"] <- "yield"


names(mariensee1517)[names(mariensee1517) == "distance_to_tree_strip"] <- "distance_to_tree_row"
names(mariensee1517)[names(mariensee1517) == "longitude"] <- "long"
names(mariensee1517)[names(mariensee1517) == "latitude"] <- "lat"
mariensee1517$tree_species <- "willow"
mariensee1517$yield_unit <- "t/ha"
names(mariensee1517)

# check:
colSums(is.na(mariensee1517)) # 12/24/48 - different
nrow(mariensee1517) # 108



# Reiffenhausen -----------------------------------------------------------
reiffenhausen16 <- read_csv("data/BONARES_Reiffenhausen16/signal.ID_7039_REIFFENHAUSEN_BIOMASS_DATA_V2.csv")

# Gladbacherhof -----------------------------------------------------------
gladbacherhof <- readxl::read_excel("data/ZALF_Hessen_2122/AFGH1_Yield_All.xlsx")


# To Do After -------------------------------------------------------------

# unite observations for Koch, because currently those are point measurements 
# not sure about gladbacherhof if it is the same for them 
