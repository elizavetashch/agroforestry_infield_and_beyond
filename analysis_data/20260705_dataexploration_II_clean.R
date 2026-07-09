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

# Signal =============================================================================
# Data structure: SIGNAL 16
#   field: Wendhausen, Dornburg, Mariensee, Forst 
#   ACS Design  — plot (r1:r4) 20 forestry and 4 control sites 
#   year        — 2016, 2017
#   nrow        - 174
#   nrow per year - w: 20, d: 20, f: 20, m16: 24, m17: 30 
# =============================================================================

signal16 <- read_csv("data/BONARES_SIGNAL16/signal.ID_7048_BIOMASSES_SIGNAL_PROJECT_V1_APR_08_2020.csv")

signal16 <- janitor::clean_names(signal16)

signal16$data_id <- "signal16"
names(signal16)[names(signal16) == "harvest_year"] <- "year"
signal16$yield_unit <- "g/m2 per year"
names(signal16)[names(signal16) == "product_crop_grass_row_and_mono"] <- "crop"
names(signal16)[names(signal16) == "straw_or_grass_dry_mass_2016"] <- "yield_straw"
names(signal16)[names(signal16) == "grain_corn_or_grass_dry_mass_16"] <- "yield"
signal16 <- signal16[, c(1:5, 9:12)]
signal16 <- signal16 %>%
  mutate(distance_to_tree_strip = as.numeric(str_extract(id, "\\d+(?=m)")))

# look
names(signal16)
head(signal16)

# check 
colSums(is.na(signal16)) 
nrow(signal16) # 216

# exclude NA yield at the tree row (T or 0m)
signal16 <- signal16[!(is.na(signal16$yield_straw) & is.na(signal16$yield)), ]

# check 
colSums(is.na(signal16)) 
nrow(signal16) # 174

# subset
levels(as.factor(signal16$site))
dornburg <- subset(signal16, site == "Dornburg")
forst   <- subset(signal16, site == "Forst")
wendhausen   <- subset(signal16, site == "Wendhausen")
mariensee <- subset(signal16, grepl("Mariensee", site))

nrow(dornburg) # 40
table(dornburg$year) # 20 2016, 20 2017

nrow(forst) # 40
table(forst$year) # 20 2016, 20 2017

nrow(wendhausen) # 40
table(wendhausen$year) # 20 2016, 20 2017
sum(duplicated(wendhausen$yield))

nrow(mariensee) # 54
table(mariensee$year) # 24 2016, 30 2017: in 2017 included 24m, in 2016 only 1m, 4m, 7m

# field assignment
dornburg$field <- "Dornburg"
forst$field <- "Forst"
wendhausen$field <- "Wendhausen"
mariensee$field <- "Mariensee"

# change yield and straw for mariensee, since it is grassland 
names(mariensee)[names(mariensee) == "yield_straw"] <- "yield_grass"
names(mariensee)[names(mariensee) == "yield"] <- "yield_straw"
names(mariensee)[names(mariensee) == "yield_grass"] <- "yield"

# check 
colSums(is.na(dornburg)) 
nrow(dornburg) # 40

# check 
colSums(is.na(forst)) 
nrow(forst) # 40

# check 
colSums(is.na(wendhausen)) 
nrow(wendhausen) # 40

# check 
colSums(is.na(mariensee)) 
nrow(mariensee) # 54

field1617 <- bind_rows(mariensee, forst, wendhausen, dornburg)

# check 
colSums(is.na(field1617)) 
nrow(field1617) # 174
table(field1617$site)

write.csv(field1617, file = "data/AnalysisData/20260702_fields1617.csv", row.names = FALSE)


# Wendhausen =============================================================================
# Data structure: Wendhausen
#   field: Wendhausen
#   ACS Design  — plot (r1:r4) 20 forestry and 4 control sites 
#   year        — year
#   distance to tree row     — "distance_to_tree_strip"
#   yield — "or_dm"    "or_dm_content"  "or_1000seed" "or_crude_fat" "ww_dm" "ww_dm_content" "ww_1000seed" 
#            "ww_crude_protein"       "decomposition"          "litter_dm"              "wood_yield_estimated"
#   coordinates - lat and lon
# =============================================================================

# Wendhausen 19 20 --------------------------------------------------------

wendhausen_1920 <- read_csv("data/BONARES_Cropland agroforestry 2019-2020/signal.ID_7069_DATEN_WENDHAUSEN_2019_20.csv")
wendhausen_1920$data_id <- "wendhausen1920"
wendhausen_1920$yield_unit <- "t/ha"
wendhausen_1920 <- janitor::clean_names(wendhausen_1920)
names(wendhausen_1920)

wendhausen_1920 <- wendhausen_1920[, -(10:15)]
wendhausen_1920$crop = "silage maize"
names(wendhausen_1920)[names(wendhausen_1920) == "sm_dm"] <- "yield"

# some rows have na in yield, because there only wood was harvested -> remove
wendhausen_1920 <- wendhausen_1920[!(is.na(wendhausen_1920$yield)), ]


# check:
colSums(is.na(wendhausen_1920)) # 8 in distance those are controls
nrow(wendhausen_1920) # 40: 20 per year 

# bind rows
wendhausen$lat <- 52.33336
wendhausen$lon <- 10.63236
names(wendhausen)[names(wendhausen) == "id"] <- "plot"
wendhausen_x1 <- bind_rows(wendhausen,wendhausen_1920)

# check:
colSums(is.na(wendhausen_x1)) # 16 in distance : 4 control 4 years
nrow(wendhausen_x1) # 80: 20 per year 
table(wendhausen_x1$year)

# Wendhausen 21 --------------------------------------------------------

wendhausen_21 <- read_csv("data/BONARES_Cropland agroforestry 2021/signal.ID_7070_DATEN_WENDHAUSEN_2021.csv")
wendhausen_21$data_id <- "wendhausen21"
wendhausen_21$yield_unit <- "t/ha"
wendhausen_21 <- janitor::clean_names(wendhausen_21)
names(wendhausen_21)

names(wendhausen_21)[names(wendhausen_21) == "sb_dm_straw"] <- "yield_straw"
names(wendhausen_21)[names(wendhausen_21) == "sb_dm"] <- "yield"
wendhausen_21$crop <- "summer barley"

names(wendhausen_21)
wendhausen_21 <- wendhausen_21[, -(11:15)]

# some rows have na in yield, because there only wood was harvested -> remove
wendhausen_21 <- wendhausen_21[!(is.na(wendhausen_21$yield)), ]

# check:
colSums(is.na(wendhausen_21)) # 4 in distance those are controls
nrow(wendhausen_21) # 20

# bind rows
wendhausen_x2 <- bind_rows(wendhausen_x1,wendhausen_21)

# check:
colSums(is.na(wendhausen_x2)) #
nrow(wendhausen_x2) # 100
table(wendhausen_x2$year) # 20 per year 

# Wendhausen 22 --------------------------------------------------------

wendhausen_22 <- read_csv("data/BONARES_Cropland agroforestry 2022/signal.ID_7071_DATEN_WENDHAUSEN_2022.csv")
wendhausen_22$data_id <- "wendhausen22"
wendhausen_22$yield_unit <- "t/ha"

wendhausen_22 <- janitor::clean_names(wendhausen_22)
names(wendhausen_22)

wendhausen_22 <- wendhausen_22[, -(10:14)]
wendhausen_22$crop = "oil rape"
names(wendhausen_22)[names(wendhausen_22) == "or_dm"] <- "yield"

# some rows have na in yield, because there only wood was harvested -> remove
wendhausen_22 <- wendhausen_22[!(is.na(wendhausen_22$yield)), ]

# check:
colSums(is.na(wendhausen_22)) # 4, since only one year 
nrow(wendhausen_22) # 20, since only one year 

wendhausen_x3 <- bind_rows(wendhausen_x2,wendhausen_22)

# check:
colSums(is.na(wendhausen_x3)) # 36 
nrow(wendhausen_x3) # 120
table(wendhausen_x3$year) # 20 per year 

# Wendhausen 23 --------------------------------------------------------

wendhausen_23 <- read_csv("data/BONARES_Cropland agroforestry 2023/signal.ID_7077_DATEN_WENDHAUSEN_2023_CR.csv")
wendhausen_23$data_id <- "wendhausen23"
wendhausen_23$yield_unit <- "t/ha"

wendhausen_23 <- janitor::clean_names(wendhausen_23)
names(wendhausen_23)

wendhausen_23 <- wendhausen_23[, -(10:14)]
wendhausen_23$crop = "sillage maize"
names(wendhausen_23)[names(wendhausen_23) == "sm_dm"] <- "yield"

# some rows have na in yield, because there only wood was harvested -> remove
wendhausen_23 <- wendhausen_23[!(is.na(wendhausen_23$yield)), ]

# check:
colSums(is.na(wendhausen_23)) # 4, since only one year 
nrow(wendhausen_23) # 24, since only one year 

wendhausen_x4 <- bind_rows(wendhausen_x3,wendhausen_23)

# check:
colSums(is.na(wendhausen_x4)) # 36 
nrow(wendhausen_x4) # 140
table(wendhausen_x4$year) # 20 per year 


# Wendhausen 2016-2023 ----------------------------------------------------

# ORGANIZE AND RENAME
wendhausen_x4$tree_species <- "poplar"
wendhausen_x4$field <- "Wendhausen"

# check:
colSums(is.na(wendhausen_x4)) # 28 NA in distance : 7 years 4 controls 
nrow(wendhausen_x4) # 140
table(wendhausen_x4$year) # 20 per year 

write.csv(wendhausen_x4, file = "data/AnalysisData/20260702_wendhausen.csv", row.names = FALSE)


# Dornburg =============================================================================
# Data structure: Dornburg 1823
#   field: Dornburg
#   ACS Design  — plot, 
#   year        — year
# =============================================================================


# Dornburg 18 23 ----------------------------------------------------------------

dornburg1823 <- read_csv("data/BONARES_DornburgVechta1823/signal.ID_7088_CROP_YIELD.csv")

dornburg1823 <- janitor::clean_names(dornburg1823)
dornburgvechta <- dornburg1823

names(dornburg1823)
head(dornburg1823)

dornburg1823 <- dornburg1823 %>%
  mutate(distance_to_tree_strip = as.numeric(str_extract(id, "\\d+(?=m)")))


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
dornburg1823[(is.na(dornburg1823$distance_to_tree_strip)), ] # its all control treatments 
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
table(dornburg1823$year) #    24   48   48   56   56   52  

# 2018: 4 distances, 4 plots, 4 controls
#       also D_AFcrop_r1 -> exlude those 
# 2019: same 20 but reduced and normal fertilization -> 40
# 2020: same
z <- dornburg1823[dornburg1823$year == 2021, ]
# 2021: in the Dornburg normal additional 18 m distance "D_AF_r1_18m", 
#       also D_AFcrop_r1 -> exlude those 

dornburg1823 <- dornburg1823[!grepl("AFcrop", dornburg1823$id), ]

# check
table(dornburg1823$year) #  20 2018,   40 2019,   40 2020,  48 2021 ,  48 2022,  44 2023

# 2023: sometimes 7 m is missing 

# bind rows
names(dornburg)[names(dornburg) == "id"] <- "plot"
names(dornburg1823)[names(dornburg1823) == "id"] <- "plot"
dornburg1623 <- bind_rows(dornburg,dornburg1823)

# lat lon (known from a publication)
dornburg1623$lat <- 51.011111
dornburg1623$lon <- 11.646111
dornburg1623$tree_species <- "poplar"
# check:
colSums(is.na(dornburg1623)) # 2 in yield, 52 in distance to tree row 
nrow(dornburg1623) # 280
table(dornburg1623$year) #  20 2016,  20 2017,  20 2018, 40 2019,  40  2020, 48  2021, 48 2022,  44 2023

# save the full 2016-2023 series 
write.csv(dornburg1623, file = "data/AnalysisData/20260701_dornburg.csv", row.names = FALSE)

# Vechta =============================================================================
# Data structure: Dornburg 1823
#   field: Vechta
#   ACS Design  — plot 
#   year        — year
# =============================================================================

vechta <- dornburgvechta[dornburgvechta$site == "Vechta", ] # 120 
names(vechta)[names(vechta) == "harvest_year"] <- "year"
names(vechta)[names(vechta) == "grain_or_corn_dry_mass"] <- "yield"
names(vechta)
head(vechta)

vechta <- vechta %>%
  mutate(distance_to_tree_strip = as.numeric(str_extract(id, "\\d+(?=m)")))


vechta$data_id <- "dornburg1823"
vechta$tree_species <- "poplar"
vechta$yield_unit <- "g/m2"

names(vechta)
head(vechta)

vechta <- vechta[, c(1, 3:6, 8:12)]
vechta$field <- "Vechta"


table(vechta$year) # 20 in 2019, 24 in 2020, 28 in 2021, 24 in 2022, 24 in 2023

# AFcrop is weighted by the area coverage of the sampling distances in AF
# exclude those 
nrow(vechta) # 120
vechta[grepl("AFcrop", vechta$id), ] # 20
vechta <- vechta[!grepl("AFcrop", vechta$id), ]
nrow(vechta) # 100

table(vechta$year) # 16 in 2019, 20 in 2020, 24 in 2021, 20 in 2022, 20 in 2023
# in 2019 only three distances - 1, 7 and 24 m 
# in 2021 very weird V_AF_r1_18m, 4 observations from the same r1_18m 
# leave them for now 

names(vechta)[names(vechta) == "id"] <- "plot"

# lat lon from publication
vechta$lat <- 52.758056
vechta$lon <- 8.534722
# check
head (vechta)
colSums(is.na(vechta)) # 20 in distance -> 5 years 4 controls 
nrow(vechta) # 100

write.csv(vechta, file = "data/AnalysisData/20260702_vechta.csv", row.names = FALSE)

# Reiffenhausen =============================================================================
# Data structure: Reiffenhausen
#   field: Reiffenhausen
#   ACS Design  — plot 
#   year        — year
# =============================================================================


reiffenhausen <- read_csv("data/BONARES_Reiffenhausen16/signal.ID_7039_REIFFENHAUSEN_BIOMASS_DATA_V2.csv")
reiffenhausen <- janitor::clean_names(reiffenhausen)

# check
nrow(reiffenhausen) # 20
colSums(is.na(reiffenhausen))

reiffenhausen <- reiffenhausen[, c(1:6, 11:12)]

names(reiffenhausen)

reiffenhausen <- pivot_longer(
  reiffenhausen,
  cols = c(straw_2016_winter_barley,
           corn_2016_winter_barley,
           straw_2017_rapeseed,
           corn_2017_rapeseed),
  names_to = c(".value", "year", "crop"),
  names_pattern = "(straw|corn)_(\\d{4})_(.*)"
)

nrow(reiffenhausen) #40

names(reiffenhausen)[names(reiffenhausen) == "corn"] <- "yield"
names(reiffenhausen)[names(reiffenhausen) == "straw"] <- "yield_straw"
names(reiffenhausen)[names(reiffenhausen) == "signal_code"] <- "plot"

# distance to tree strip 
reiffenhausen <- reiffenhausen %>%
  mutate(distance_to_tree_strip = as.numeric(str_extract(distance_from_tree_row, "\\d+(?=m)")))

head(reiffenhausen)
nrow(reiffenhausen) # 40

reiffenhausen <- reiffenhausen[!is.na(reiffenhausen$yield), ]
reiffenhausen <- reiffenhausen[, -c(2:3)]

# check
nrow(reiffenhausen) # 32
colSums(is.na(reiffenhausen)) # distance to tree strip 8 -> 2 years of control
head(reiffenhausen)


reiffenhausen$data_id <- "reiffenhausen16"
reiffenhausen$tree_species <- "poplar"
reiffenhausen$yield_unit <- "g/m2 per year"
reiffenhausen$field <- "Reiffenhausen"
# lat lon from publications
reiffenhausen$lat <- 51.666667
reiffenhausen$lon <- 9.983333

names(reiffenhausen)[names(reiffenhausen) == "id"] <- "plot"

head(reiffenhausen)

write.csv(reiffenhausen, file = "data/AnalysisData/20260702_reiffenhausen.csv", row.names = FALSE) 


# Mariensee =============================================================================
# Data structure: Mariensee
#   field: Mariensee
#   ACS Design  — plot 
#   year        — year
# =============================================================================

# decided not to merge the dataset, because the values differ in the year 2017, and i dont know why

names(mariensee)[names(mariensee) == "id"] <- "plot"

mariensee1719 <- read_csv("data/Mariensee1719/signal.ID_7041_BIOMASSE_17_18_MS_280319.csv")
mariensee1719$data_id <- "mariensee1719"

mariensee1719 <- janitor::clean_names(mariensee1719)
names(mariensee1719)

# compare overlap with signal16 mariensee in 2017 -> this is why they are kept separate
mariensee$yield_tha <- mariensee$yield/100
mariensee$plot_join <- gsub("_", "-", gsub("_[0-9]+m$", "", mariensee$plot))

comparison <- mariensee %>%
  filter(year == 2017) %>%
  select(plot_join, yield_tha, distance_to_tree_strip) %>%
  inner_join(
    mariensee1719 %>%
      filter(year == 2017) %>%
      select(plot, grass_dm, distance_to_tree_strip),
    by = c("plot_join" = "plot", "distance_to_tree_strip")
  ) %>%
  mutate(equal = yield_tha == grass_dm)

mariensee1719 <- mariensee1719[, c(3:10, 15)]
mariensee1719$crop = "grass"
names(mariensee1719)[names(mariensee1719) == "grass_dm"] <- "yield"

names(mariensee1719)[names(mariensee1719) == "longitude"] <- "lon"
names(mariensee1719)[names(mariensee1719) == "latitude"] <- "lat"
mariensee1719$tree_species <- "willow"
mariensee1719$yield_unit <- "t/ha"
mariensee1719$field <- "Mariensee"
names(mariensee1719)

# check:
colSums(is.na(mariensee1719)) # 24 in yield
nrow(mariensee1719) # 84

# some rows have na in yield, because there only wood was harvested -> remove
mariensee1719 <- mariensee1719[!(is.na(mariensee1719$yield)), ]

# check:
colSums(is.na(mariensee1719)) # 0 in yield, 12 in distance: 2 years with 6 controls 
nrow(mariensee1719) # 60: 2 years with 6 plots, 4 distances 4 times 

# check:
nrow(mariensee1719[mariensee1719$year == 2017, ]) # 30
nrow(mariensee1719[mariensee1719$year == 2018, ]) # 30

write.csv(mariensee1719, file = "data/AnalysisData/20260702_mariensee.csv", row.names = FALSE) 

# Forst =============================================================================
# Data structure: Forst1920
#   field: Forst
#   ACS Design  — plot 
#   year        — year
# =============================================================================

forst1920 <- read_csv("data/BONARES_Forst1920/signal.ID_7060_CROP_YIELDS_FORST_2019_2020.csv")
forst1920$data_id <- "forst1920"

forst1920 <- janitor::clean_names(forst1920)
names(forst1920)
head(forst1920)

forst1920 <- forst1920[, c(2:6, 14, 19:20)]
names(forst1920)

# latitude longitude
names(forst1920)[names(forst1920) == "x"] <- "lon"
names(forst1920)[names(forst1920) == "y"] <- "lat"

# distance to tree row
head(forst1920$signal_code)
forst1920 <- forst1920 %>%
  mutate(distance_to_tree_strip = as.numeric(str_extract(signal_code, "\\d+(?=m)")))

head(forst1920)

# crop and yield 
names(forst1920)[names(forst1920) == "corn_dry_weight_t_per_ha"] <- "yield"
names(forst1920)[names(forst1920) == "straw_dry_weight_t_per_ha"] <- "yield_straw"
names(forst1920)[names(forst1920) == "crop_type"] <- "crop"

# date to year
forst1920 <- forst1920 %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  select(-date) 

# add columns
forst1920$field <- "Forst"
forst1920$yield_unit <- "t/ha"



# check 
nrow(forst1920) # 40
colSums(is.na(forst1920)) # 8 in distance
table(forst1920$year) # 20 and 20 


# bind rows with forst from signal16
head(forst1920)
head(forst)
names(forst)[names(forst) == "id"] <- "plot"
names(forst1920)[names(forst1920) == "signal_code"] <- "plot"

# check 
nrow(forst) # 40
colSums(is.na(forst)) # 8 in distance
table(forst$year) # 20 and 20 
# add lat lon column based on the forst1920 values
forst$lon <- 14.633
forst$lat <- 51.787

forst1620 <- bind_rows(forst, forst1920)

# tree species from publicaitons
forst1620$tree_species <- "poplar"

# check 
nrow(forst1620) # 80
colSums(is.na(forst1620)) # 16 in distance
table(forst1620$year) # 20 four times

write.csv(forst1620, file = "data/AnalysisData/20260702_forst.csv", row.names = FALSE) 

# Gladbacherhof =============================================================================
# Data structure: gladbacherhof
#   field: Gladbacherhof
#   ACS Design  — plot 
#   year        — year
#   coordinates - lat lon
# =============================================================================

gladbacherhof <- readxl::read_excel("data/ZALF_Hessen_2122/AFGH1_Yield_All.xlsx")
gladbacherhof <- janitor::clean_names(gladbacherhof)

names(gladbacherhof)
head(gladbacherhof)
colSums(is.na(gladbacherhof)) 

gladbacherhof$data_id <- "gladbacherhof2122"
gladbacherhof$field <- "Gladbacherhof"
gladbacherhof$yield_unit <- "kg/m2"
names(gladbacherhof)[names(gladbacherhof) == "grain_kg_m2"] <- "yield"
names(gladbacherhof)[names(gladbacherhof) == "biomass_kg_m2"] <- "yield_straw"
names(gladbacherhof)[names(gladbacherhof) == "total_kg_m2"] <- "yield_total"
names(gladbacherhof)[names(gladbacherhof) == "distance"] <- "distance_to_tree_strip"
gladbacherhof$distance_to_tree_strip <- as.numeric(gladbacherhof$distance_to_tree_strip) # 12 NAs will be introduced, those are control values
names(gladbacherhof)[names(gladbacherhof) == "db_site_name"] <- "plot"

# date to year
gladbacherhof <- gladbacherhof %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  select(-date) 


gladbacherhof <- gladbacherhof[, c(1:4,6:7,10:20)]

# check 
table(gladbacherhof$year) # 162 in 2021, 150 in 2022
table(gladbacherhof$year, gladbacherhof$transect) # A,B,C,D in 2021 and 2022 each 36, in 2022 additional E transect 12 
table(gladbacherhof$year, gladbacherhof$site) # 6 control measurements each year 

levels(as.factor(gladbacherhof$row)) # 6 tree rows
levels(as.factor(gladbacherhof$transect)) # 5 transects (4 each tear, 1 add in 2022)
nrow(gladbacherhof) # 312: 4 transect, 3 distance, 2 directions, 6 tree rows, 2 years, 12 controls, 12 from an additional transect (E in 2022)

colSums(is.na(gladbacherhof)) 
head(gladbacherhof, n=10)
nrow(gladbacherhof) # 312

# gladbacherhof has a similar design with transects, and up and down directions
# there are 3 distances : 2.5, 6 and 10.5 m 
# in comparision to other datasets it has 6 tree rows, so 24 locations 
# other datasets had 2 rows

# Gladbacherhof has a mixed tree design 
# # suplementary material section:
# The tree strips represent a successional, multifunctional woody polyculture (Lovell et al., 2018), designed as a within- and between-row diversified system with biomass (Populus spp., five different clones), high-value timber (Juglans regia, Pyrus communis, Prunus avium, Sorbus domestica and Sorbus torminalis), fruit trees (Malus domestica, 4 different varieties) and Sambucus nigra as understory shrubs (Figure S2A). 

gladbacherhof$tree_species <- "mixed"
write.csv(gladbacherhof, file = "data/AnalysisData/20260703_gladbacherhof.csv", row.names = FALSE) 


# Koch 25 =============================================================================
# Data structure: Koch 25
#   field: Ihinger Hof 
#   ACS Design  — block, id
#   year        — repeated sampling across time
#   p_dist      — distance to measurement point (blocking factor)
#   yield_wweight — response variable
#   tree - hedge, willow, walnut (treatment column)
# =============================================================================

koch25 <- readr::read_delim("data/Koch25/yields_wintercrops.csv", delim = ";", locale = locale(decimal_mark = ","))
lat <- mean(koch25$Lat)
lon <- mean(koch25$Long)
koch25 <- janitor::clean_names(koch25)


names(koch25)[names(koch25) == "treatment"] <- "tree_species"

names(koch25)[names(koch25) == "p_dist"] <- "distance_to_tree_strip"
names(koch25)[names(koch25) == "yield_wweight"] <- "yield"

names(koch25)
head(koch25)

summary(koch25$distance_to_tree_strip)

koch25 <- koch25 %>% 
  mutate(distance_to_tree_strip = case_when(distance_to_tree_strip < 1.1 ~ 1,
                                            distance_to_tree_strip < 4.1 ~ 4,
                                            distance_to_tree_strip < 7.1 ~ 7,
                                            distance_to_tree_strip < 12.1 ~ 12,
                                            distance_to_tree_strip < 18.1 ~ 18,
                                            distance_to_tree_strip > 18.1 ~ 24))

summary(koch25$distance_to_tree_strip)

# check:
colSums(is.na(koch25)) # 0
nrow(koch25) # 28174
head(koch25, n = 25)

# calculate means per id and block

koch25$plot <- paste(
  koch25$id,
  koch25$block,
  koch25$distance_to_tree_strip,
  sep = "_"
)

koch25 <- koch25 %>%
  group_by(plot, year, tree_species, aspect, crop, distance_to_tree_strip) %>%
  summarise(
    mean_yield = mean(yield, na.rm = TRUE),
    sd_yield   = sd(yield, na.rm = TRUE),
    n          = n(),
    .groups = "drop"
  ) %>% 
  mutate(yield = mean_yield) %>% 
  select(-mean_yield)

# check
head(koch25)
colSums(is.na(koch25)) # 0
nrow(koch25) # 667

table(koch25$year) # 96 2012, 101 2014, 90 2016, 102 2018, 100 2021, 88 2022, 100 2023 

tapply(koch25$plot, koch25$year, function(x) length(unique(x)))
tapply(koch25$tree_species, koch25$year, function(x) length(unique(x)))

koch25$data_id <- "koch25"
koch25$field <- "IhingerHof"
koch25$lat <- lat
koch25$lon <- lon

koch25$yield_unit <- "t/ha" # known from the publicaiton

koch_ab16 <- (koch25[(koch25$year > 2015), ])
write.csv(koch_ab16, file = "data/AnalysisData/20260705_koch1623.csv", row.names = FALSE) 
write.csv(koch25, file = "data/AnalysisData/20260705_koch1223.csv", row.names = FALSE) 


# =============================================================================
# Merge all fields
#   note: units still differ (g/m2, t/ha, kg/m2) and koch25 uses mean_yield
#   instead of yield, so this is a structural merge, not a harmonized one
#   mariensee 2016-2017 (signal16 part) is left out on purpose, see above
# =============================================================================


all_fields <- bind_rows(
  wendhausen_x4,
  dornburg1623,
  forst1620,
  vechta,
  reiffenhausen,
  mariensee1719,
  gladbacherhof,
  koch_ab16
)

# check
nrow(all_fields)
table(all_fields$field)

write.csv(all_fields, file = "data/AnalysisData/20260705_all_fields.csv", row.names = FALSE)