# VARIABLES                 GOOD DEFAULT
# 
# 1 continuous              histogram
# 1 categorical             bar chart
# 
# continuous + continuous   scatterplot
# 
# categorical + continuous  boxplot
# jitter/dot plot
# violin plot
# 
# categorical + categorical heatmap
# grouped/stacked bar
# 
# time + continuous         line plot
# time + categorical        tile plot


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

fieldpalette = 
  c("#577590",
   "#4d908e",
   "#43aa8b",
  "#90be6d",
   "#f9c74f",
   "#f8961e",
   "#f3722c",
   "#f94144")


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
o <- df |> distinct(
  field, year, fieldlength, l_contag, l_sidi,
  l_shdi,l_np , l_ai, l_ed, swf_year, prop_swf_within,
  latitude, longitude, temp_C_mean, sun_MJ_m2_mean, 
  precip_mm_sum, min_slope, mean_slope, max_slope,
  clay, sand, silt, treeage, AFage
)


# YEAR OF FIELDS 
o |>
  select(field, year) |>
  distinct() |>
  mutate(present = TRUE) |>
  complete(
    field,
    year = seq(min(year), max(year))
  ) |>
  mutate(present = replace_na(present, FALSE)) |> 
  ggplot(aes(x = year, y = field, fill = present)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c(`FALSE` = "white", `TRUE` = "#586f7c"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(min(dat$year), max(dat$year))) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )

# FIELDLENGTH
o |> 
  select(field, fieldlength) |>
  distinct() |> 
  ggplot(aes(x = field, y = fieldlength, color = field)) +
  geom_point(size = 10) +
  scale_color_manual(
    values = setNames(fieldpalette, unique(o$field))
  ) +
  theme_light()

# relative values 

rv <- o |>
  select(field, year, fieldlength, l_contag, l_sidi, l_shdi, l_np, l_ai, l_ed,
         prop_swf_within, temp_C_mean, sun_MJ_m2_mean, precip_mm_sum,
         min_slope, mean_slope, max_slope, clay, sand, silt, treeage, AFage) |>
  pivot_longer(
    -c(field, year),
    names_to = "variable",
    values_to = "value"
  ) |>
  group_by(year, variable) |>
  mutate(
    value_scaled = scales::rescale(value, to = c(0, 1))
  ) |>
  ungroup() |>
  mutate(variable = factor(variable, levels = c(
    "fieldlength", "AFage", "treeage",
    "temp_C_mean", "precip_mm_sum", "sun_MJ_m2_mean",
    "min_slope", "clay", "silt", "sand", "mean_slope", "max_slope",
    "l_contag", "l_sidi", "l_shdi", "l_np", "l_ai", "l_ed",
    "prop_swf_within"
  ))) |>
  ggplot(aes(x = variable, y = field, fill = value_scaled)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(
    name = "Relative value",
    limits = c(0, 1)
  ) +
  facet_wrap(~ year, ncol = 2, nrow = 4) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(
      size = 12
    ),
    axis.text.x = element_text(
      angle = 90,
      hjust = 0.5,
      vjust = 0.5,
      size = 12
    ),
    legend.position = "none"
  )

ggsave(plot = rv, file = "03_plots/field_variables_by_year.png", width = 10, height = 14, dpi = 300)


# CORRELATION PLOTS 

groups <- list(
  tree_characteristics = c("treeage", "AFage"),
  
  climate = c(
    "lat", "long", "temp", "sun", "precip",
    "min_slope", "mean_slope", "max_slope",
    "clay", "sand", "silt"
  ),
  
  landscape = c(
    "fieldlength", "contag", "simpson", "shannon",
    "patchnumber", "aggregation", "edgedensity"
  ),
  
  small_woody_features = c(
    "swf_distance", "swf_prop", "swf_field"
  )
)

# Combine variables into one data frame
cor_data <- data.frame(
  treeage, AFage,
  lat, long, temp, sun, precip,
  min_slope, mean_slope, max_slope,
  clay, sand, silt,
  fieldlength, contag, simpson, shannon,
  patchnumber, aggregation, edgedensity,
  swf_distance, swf_prop, swf_field
)

# Load packages
library(ggplot2)
library(GGally)

# Create and save one correlation plot per group
for (g in names(groups)) {
  
  vars <- groups[[g]]
  
  p <- ggpairs(
    cor_data[, vars, drop = FALSE],
    upper = list(
      continuous = wrap("cor", size = 4)
    ),
    lower = list(
      continuous = wrap("points", alpha = 0.6, size = 1.5)
    ),
    diag = list(
      continuous = wrap("densityDiag", alpha = 0.5)
    )
  ) +
    ggtitle(paste("Correlations within", g)) +
    theme_minimal()
  
  # Save plot
  ggsave(
    filename = file.path(
      "03_plots",
      paste0("correlations_", g, ".png")
    ),
    plot = p,
    width = 12,
    height = 10,
    dpi = 300
  )
  
  # Also display plot
  print(p)
}


# How many different parameters and yield values do I have? ---------------


# original values #############################
o <- df |> distinct(
  field, year, fieldlength, l_contag, l_sidi,
  l_shdi,l_np , l_ai, l_ed, swf_year, prop_swf_within,
  latitude, longitude, temp_C_mean, sun_MJ_m2_mean,
  precip_mm_sum, min_slope, mean_slope, max_slope,
  clay, sand, silt, treeage, AFage
)
nrow(o) # 34


# yield values #############################

y <- df |> distinct(field, year, crop_unified, distance_to_tree_strip, yield_tha)

x <- df |> distinct(field, year, crop_unified, distance_to_tree_strip, yield_tha, ID_nodist)
xs <- df |> distinct(field, year, crop_unified, distance_to_tree_strip, yield_tha, ID_nodist, swf_year)

nrow(xs) # 1171
nrow(y) # 1158
nrow(x) # 1171

comparison <- x |>
  group_by(field, year, crop_unified, distance_to_tree_strip, yield_tha) |>
  summarise(
    n_ID_nodist = n_distinct(ID_nodist),
    ID_nodist = paste(unique(ID_nodist), collapse = ", "),
    .groups = "drop"
  ) |>
  filter(n_ID_nodist > 1)

comparison # all good, all okay


# relationship in yield: 
# for each distance to tree strip there is a value of yield 

plot(x$yield_tha ~ x$distance_to_tree_strip)
# it is a functional relationship 

# continous predictor variable #############################

s <- df |> distinct(field, swf_year, prop_swf, distance, ID_nodist)

nrow(s) # 180

plot(s$prop_swf ~ s$distance)
# also a functional relationship


# SPATIAL AUTOCORRELATION ----------------------------------------

library(tidyverse)

df <- df |>
  mutate(
    # remove "_4.5m_2021" / "_7m_2018" / "_24m_2016" style suffixes
    id_prefix = str_remove(ID_nodist, "_\\d{4}$"),
    
    # remove field name from the front, plus any trailing separator
    transect_raw = str_remove(
      id_prefix,
      regex(str_c("^", str_replace_all(field, "([\\.\\+\\*\\?\\^\\$\\{\\}\\(\\)\\|\\[\\]\\\\])", "\\\\\\1")))
    ),
    transect_raw = str_remove(transect_raw, "^[A-Z]+"),
    transect_raw = str_remove(transect_raw, "^[_\\-]"),  # clean leading _ or -
    transect_raw = str_remove(transect_raw, "^[A-Z]+"),
    transect_raw = str_remove(transect_raw, "^[_\\-]")  # clean leading _ or -
  )

# sanity check before going further
df |> distinct(ID_nodist, field, id_prefix, transect_raw)

df <- df |>
  group_by(field, year) |>
  mutate(
    transect_n = as.integer(factor(transect_raw))  # 1:n within field x year
  ) |>
  ungroup()

df |>
  filter(field!="Gladbacherhof") |> 
  distinct(field, year, crop_unified, distance_to_tree_strip, transect_n, yield_tha) |>
  ggplot(aes(x = distance_to_tree_strip, y = factor(transect_n), color = yield_tha)) +
  geom_point(size = 8) +
  facet_grid(field ~ crop_unified, scales = "free_y") +
  #scale_colour_manual() +
  labs(
    x     = "Distance to tree strip (m)",
    y     = "Transect",
    fill  = "Yield (t/ha)",
    title = "Yield by transect and distance — spatial check"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text.y = element_text(angle = 0))

df |>
  filter(field=="Gladbacherhof") |> 
  distinct(field, year, crop_unified, distance_to_tree_strip, transect_n, yield_tha) |>
  ggplot(aes(x = distance_to_tree_strip, y = factor(transect_n), color = yield_tha)) +
  geom_point(size = 8) +
  facet_grid(field ~ crop_unified, scales = "free_y") +
  scale_fill_viridis_c(option = "magma", na.value = "grey80") +
  labs(
    x     = "Distance to tree strip (m)",
    y     = "Transect",
    fill  = "Yield (t/ha)",
    title = "Yield by transect and distance — spatial check"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text.y = element_text(angle = 0))
