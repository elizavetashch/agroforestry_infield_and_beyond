

# file start date: 9 Sep 2026
# file finish date: 

# Author: Elizaveta Shcherbinina 


# Data exploration of teh current data set and modelling

# ckeck for spatial autocorrelation in each field 
# for the ihinger hof it is possible to take the x and y coord
# for the others one would have to work with transects 

# correlation plot of the variables

# choose numeric variables and build group matrices for the groups, check the correlations within groups 

# build models 

# m1: yield ~ distance
# m2: yield surrounding landscape
# m3: yield and surrounding landscape 


# Packages ----------------------------------------------------------------

library(ggplot2)
library(tidyverse)



# Load Data ---------------------------------------------------------------
rm(list=ls())
df <- read.csv("01_Data/dffinal_20260909.csv")
df <- df |> mutate(yield_log = log(yield_tha+2), yield_scaled = as.numeric(scale(yield_log)))
table(df$field)
colnames(df)


# factors
year <- as.factor(df$year)
id <- as.factor(df$id)
field <- as.factor(df$field)
year_treecut <- as.factor(df$harvestyear)
crop <- as.factor(df$crop_unified)

# distance predictor 
tree_distance <- as.numeric(df$distance_to_tree_strip)

# tree characteristics
treeage <- as.numeric(df$treeage)
AFage <- as.numeric(df$AFage)

# climate 
lat <- as.numeric(df$latitude)
long <- as.numeric(df$longitude)
temp <- as.numeric(df$temp_C_mean)
sun <- as.numeric(df$sun_MJ_m2_mean)
precip <- as.numeric(df$precip_mm_sum)
min_slope <- as.numeric(df$min_slope)
mean_slope <- as.numeric(df$mean_slope)
max_slope <- as.numeric(df$max_slope)
clay <- as.numeric(df$clay)
sand <- as.numeric(df$sand)
silt <- as.numeric(df$silt)

# landscape
fieldlength <- as.numeric(df$fieldlength)
contag <- as.numeric(df$l_contag)
simpson <- as.numeric(df$l_sidi)
shannon <- as.numeric(df$l_shdi)
patchnumber <- as.numeric(df$l_np)
aggregation <- as.numeric(df$l_ai)
edgedensity <- as.numeric(df$l_ed)

# small woody features
swf_year <- as.factor(df$swf_year)
swf_distance <- as.numeric(df$distance)
swf_prop <- as.numeric(df$prop_swf)
swf_field <- as.numeric(df$prop_swf_within)



# original values 
df_fieldorigin <- df |> distinct(
  field, year, fieldlength, l_contag, l_sidi,
  l_shdi,l_np , l_ai, l_ed, swf_year, prop_swf_within,
  latitude, longitude, temp_C_mean, sun_MJ_m2_mean, 
  precip_mm_sum, min_slope, mean_slope, max_slope,
  clay, sand, silt, treeage, AFage
)


# plots
df_fieldorigin %>%
  select(
    fieldlength, l_contag, l_sidi, l_shdi, l_np, l_ai, l_ed,
    prop_swf_within,
    latitude, longitude,
    temp_C_mean, sun_MJ_m2_mean, precip_mm_sum,
    min_slope, mean_slope, max_slope,
    clay, sand, silt,
    treeage, AFage
  ) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  facet_wrap(~variable, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
