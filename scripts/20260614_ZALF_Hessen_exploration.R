

# Clean teh Environment
rm(list=ls())

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

df <- readxl::read_excel("data/ZALF_Hessen_2122/AFGH1_Yield_All.xlsx")
# check fo rthe delimeter, as german data uses commas sometimes
# in this case add next line to read_delim if needed
# locale = locale(decimal_mark = ",")
# save the unchanged dataset separately 
df_orig <- df
df <- janitor::clean_names(df)

glimpse(df)
names(df)
dim(df)

plot(df) # Attention! In can take a while if you have a big dataset


# Factors -----------------------------------------------------------

df <- df %>% 
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

# Distributions -----------------------------------------------------------


library(dplyr)

sumobs <- function(factor) {
  printout <- df %>%
    group_by(.data[[factor]]) %>%
    summarise(
      min    = min(biomass_kg_m2, na.rm = TRUE),
      max    = max(biomass_kg_m2, na.rm = TRUE),
      mean   = mean(biomass_kg_m2, na.rm = TRUE),
      median = median(biomass_kg_m2, na.rm = TRUE),
      sd     = sd(biomass_kg_m2, na.rm = TRUE),
      n      = sum(!is.na(biomass_kg_m2)),
      .groups = "drop"
    )
  
  cat("\n\nFactor:", factor, "\n")
  print(printout)
}

factors <- names(df)[sapply(df, is.factor)]

lapply(factors, sumobs)


# Plots -------------------------------------------------------------------


boxplot(df$biomass_kg_m2 ~ df$crop)
boxplot(df$biomass_kg_m2 ~ df$date)

ggplot(df, aes ( x = biomass_kg_m2, color = row))



# Bremsberg4 --------------------------------------------------------------


df %>% filter(site == "Bremsberg4")

