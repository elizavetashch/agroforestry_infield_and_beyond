# Packages ----------------------------------------------------------------

pkgs <- c("ggplot2", "tidygraph", "dplyr", "tidyr", "tidyverse")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(ggraph)
library(tidygraph)
library(dplyr)
library(tidyr)
library(tidyverse)

# Read the Data -----------------------------------------------------------

df <- readr::read_delim("data/Koch25/yields_wintercrops.csv", delim = ";", locale = locale(decimal_mark = ","))
# check fo rthe delimeter, as german data uses commas sometimes
# in this case add next line to read_delim if needed
# locale = locale(decimal_mark = ",")
# save the unchanged dataset separately 
df_orig <- df

glimpse(df)
plot(df) # Attention! In can take a while if you have a big dataset

# Define Response Variable
X <- (df$Grass_DM)

# 1. Dimensions & structure
dim(df)
glimpse(df)

# 2. Missing values per column
df |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") |>
  filter(n_missing > 0) |>
  arrange(desc(n_missing))

# 3. Distinct values for key categoricals
df |> count(Site)
df |> count(Orientation)
df |> count(Soil_type)
df |> count(Plot)
df |> count(Date_)
df |> count(Distance_to_tree_strip)

# 4. Numeric summary of grass & yield variables
df |>
  select(Grass_DM, Grass_DM_content, Grass_crude_protein,
         Grass_crude_fat, Grass_sugar, Grass_ELOS, Grass_EULOS,
         Litter_DM, Wood_yield_estimated_DM, Wood_yield_measured_DM) |>
  summary()

# 5. Distribution of Litter_DM (only non-NA outcome with data)
ggplot(df, aes(x = Litter_DM)) +
  geom_histogram(bins = 30)

# 6. Litter_DM by Distance_to_tree_strip and Orientation
ggplot(df, aes(x = factor(Distance_to_tree_strip), y = Litter_DM)) +
  geom_boxplot() +
  facet_wrap(~Orientation) +
  labs(x = "Distance to tree strip (m)")

# 7. Grass_DM over time, coloured by Orientation (non-NA rows only)
df |>
  filter(!is.na(Grass_DM)) |>
  ggplot(aes(x = Date_, y = Grass_DM, colour = Orientation)) +
  geom_point() +
  facet_wrap(~Site)

