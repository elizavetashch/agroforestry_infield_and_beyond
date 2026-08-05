
rm(list = ls())
# Analysis script 
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(car)
library(car)
library(lme4)

fertilization <- read_csv("data/DataDescription_Fertilization.csv")
treeharvest <- read_csv("data/DataDescription_TreeHarvest.csv")
df <- read_csv("data/AnalysisData/20260723_all_fields.csv")
df <- df[!is.na(df$distance_to_tree_strip), ] # delete control treatments
swf <- read_csv("analysis_data/20260726_propswf.csv")

  # additional data preparation 
dfmod <- df |> mutate( harvestyear = case_when( 
  field == "Dornburg" ~ 2014, 
  field == "Forst" ~ 2014, 
  field == "Gladbacherhof" ~ 2019, 
  field == "IhingerHof" & year < 2019 ~ 2016, 
  field == "IhingerHof" & year >= 2022 ~ 2022, 
  field == "IhingerHof" & year >= 2019 ~ 2019, 
  field == "Mariensee" ~ 2015, 
  field == "Reiffenhausen" ~ 2015, 
  field == "Vechta" ~ 2018, 
  field == "Wendhausen" & year < 2021 ~ 2013, 
  field == "Wendhausen" & year >= 2021 ~ 2021, 
  TRUE ~ NA_real_ ), 
  treeage = year - harvestyear )

write.csv(dfmod, "analysis_data/20260730_AF_fields.csv", row.names = FALSE)

dfmodnomaize <- dfmod |> filter(crop_unified != "maize")
mod <- glm(yield_tha ~ distance_to_tree_strip*treeage + crop_unified, data = dfmodnomaize, family = "gaussian")

par(mfrow = c(2,2))
plot(mod)


Anova(mod, type = "II", test.statistic = "F")
summary(mod)
out.anova <- Anova(mod, test.statistic = "F")    
R2.value <- sum(out.anova[,"Sum Sq"][1:nrow(out.anova)-1]) / sum(out.anova[,"Sum Sq"])
R2.value

mod <- lmer(
  yield_tha ~ distance_to_tree_strip*treeage + crop_unified +
    (1 | field) + (1 | year), data = dfmodnomaize)


sjPlot::plot_model(mod)
sjPlot:: tab_model(mod)
summary(mod)
# H1: Position of the site 

# H2: AF Design 



# H3: Surrounding Landscape 






























df |> 
  select(year, field, crop_unified) |> 
  filter(year == 2023) |> 
  unique()


ggplot(df, aes(x=distance_to_tree_strip, y=yield_tha, color=crop_unified)) + 
  geom_point()+
  geom_smooth()

yield ~ distance + crop 


mod <- glm(yield_tha ~ distance_to_tree_strip + crop , data = df, family = "gaussian")

par(mfrow = c(2,2))
plot(mod)


Anova(mod, type = "II", test.statistic = "F")
summary(mod)
out.anova <- Anova(mod, test.statistic = "F")    
R2.value <- sum(out.anova[,"Sum Sq"][1:nrow(out.anova)-1]) / sum(out.anova[,"Sum Sq"])
R2.value

# mixed effect 

df <- df |> 
  mutate(yield_scaled = scale(yield_tha),
         distance_to_tree_strip_scaled = scale(distance_to_tree_strip))

mod <- lmer(
  yield_scaled ~ distance_to_tree_strip_scaled + crop_unified +
    (1 | field), data = df)


sjPlot::plot_model(mod)
sjPlot:: tab_model(mod)


# H1: 
# yield in agroforestry systems will differ between systems based on the environemntal conditions the fields exist in. 


# H2: 
# yield will vary in fields based on the design implemented in those fields
# - the setup of the field 
# - the variation within the field itself will differ based on teh distance from the tree strip 

# H3: 
# yield will differ based on
