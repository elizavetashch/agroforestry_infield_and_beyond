
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

df <- readr::read_delim("data/Mariensee1517/signal.ID_7008_Gras_Laub_Holz_MS_2015_2016_2017.csv", delim = ",")
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



# Filter for Corine Countries if needed -----------------------------------

corinecountries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
                     "Czechia", "Denmark", "Estonia", "Finland", "France", 
                     "Germany", "Greece", "Hungary", "Ireland", "Italy", 
                     "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", 
                     "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
                     "Spain", "Sweden", "Iceland", "Liechtenstein", "Norway", 
                     "Switzerland", "Albania", "Bosnia and Herzegovina", "Kosovo", 
                     "Montenegro", "North Macedonia", "Serbia", "Turkey")
df <- df %>% filter(Country %in% corinecountries)



# Define Response Variable
X <- (df$Yield_wweight)

# Define Predictor Variable


# Define Factor Variables 
df$Crop <- as.factor(df$Crop)
df$Aspect <- as.factor(df$Aspect)
df$Treatment <- as.factor(df$Treatment)

#  of Factor variables -----------------------------------------------------------


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



# Design Cross Tabulation -------------------------------------------------



# Step 1: Are there outliers in Y and X?  ---------------------------------

num <- sapply(df, is.numeric)

Q1 <- apply(df[, num], 2, quantile, 0.25, na.rm = TRUE)
Q3 <- apply(df[, num], 2, quantile, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

outliers <- sweep(df[, num], 2, Q1 - 1.5 * IQR, `<`) |
  sweep(df[, num], 2, Q3 + 1.5 * IQR, `>`)

colSums(outliers)
df[rowSums(outliers) > 0, ]
   
# Step 3: Are the data normally distributed? ------------------------------
hist(df$Yield_wweight)
hist(df$Year)

levels(df$Treatment)
levels(df$Aspect)
levels(df$Crop)

plot(X)
ggplot(df, aes(x = Crop, y = Yield_wweight, color = Crop)) +
  geom_boxplot() +
  theme_bw()

# Step 4: Are there lots of zeros in the data? ----------------------------

df %>% 
  summarise(across(everything(), ~ mean(is.na(.))))  %>% 
  pivot_longer(everything(), names_to = "column", values_to = "prop_missing")  %>% 
  filter(prop_missing > 0)  %>% 
  arrange(desc(prop_missing))  %>% 
  print(n = Inf)


# Step 5: Is there collinearity among the covariates? ---------------------
  

# Step 6: What are the relationships between Y and X variables? -----------


# Step 7: Should we consider interactions? --------------------------------


# Step 8: Are observations of the response variable independent? ----------



# Final Step: 

write.csv(df, "data/collection/202606XX_df.csv", row.names = FALSE)

  