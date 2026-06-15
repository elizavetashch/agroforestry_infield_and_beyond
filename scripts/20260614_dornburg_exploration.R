
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

df <- readr::read_delim("data/Bonares_Dornburg/signal.ID_7004_PROD_D_2016_V2.csv")
# check fo rthe delimeter, as german data uses commas sometimes
# in this case add next line to read_delim if needed
# locale = locale(decimal_mark = ",")
# save the unchanged dataset separately 
df_orig <- df

glimpse(df)
plot(df) # Attention! In can take a while if you have a big dataset


# Dist VS Prod ------------------------------------------------------------

ggplot(df, aes(x = Dist, y = Prod))+
  geom_boxplot()+
  theme_bw()+
  geom_point(size= 3, alpha = 0.5)+
  theme(axis.text.x = element_text(size = 13))


