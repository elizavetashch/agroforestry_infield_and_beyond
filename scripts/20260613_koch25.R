


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
X <- (df$Yield_wweight)

# Define Predictor Variable


# Define Factor Variables 
df$Crop <- as.factor(df$Crop)
df$Aspect <- as.factor(df$Aspect)
df$Treatment <- as.factor(df$Treatment)

# Step 1: Are there outliers in Y and X?  ---------------------------------
plot(X)
ggplot(df, aes(x = Crop, y = Yield_wweight, color = Crop)) +
  geom_boxplot() +
  theme_bw()

# Step 3: Are the data normally distributed? ------------------------------
hist(df$Yield_wweight)
hist(df$Year)

levels(df$Treatment)
levels(df$Aspect)
levels(df$Crop)


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


