rm(list=ls())
library(dplyr)
library(readr)
library(tidyr)

df <- read.csv("C:/Users/Elizaveta/OneDrive - Universität Bayreuth/Dokumente/MasterThesis/MA_RProject/data/AnalysisData/20260819_AFswf.csv")

# fertilization
fert_raw <- gsub("[\u2013\u2212]", "-", df$fertilization_rate_kg_n_p_k_ha_1_year_1)
df$fert_N <- as.numeric(sub("^([0-9.]+).*", "\\1", fert_raw))
df$fert_N[fert_raw == "no fertilization"] <- 0

# soilgrids clay silt sand content dataset
soil <- read.csv( ".\\data\\SoilGrids\\20260822_soiltexture.csv")

# assig the af start and the af age from the literature
df <- df |>
  mutate(year_AFplanting = case_when(
    field == "Dornburg"      ~ 2007,
    field == "Forst"         ~ 2010,
    field == "Gladbacherhof" ~ 2020,
    field == "IhingerHof"    ~ 2008,
    field == "Mariensee"     ~ 2008,
    field == "Reiffenhausen" ~ 2011,
    field == "Vechta"        ~ 2019,
    field == "Wendhausen"    ~ 2008
  )) |> 
  mutate(AFage = year - year_AFplanting)

dffull <- left_join(df, soil, join_by(field == fields))
write.csv(dffull, ".\\data\\AnalysisData\\20260823_full.csv", row.names = FALSE)

