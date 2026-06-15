


library(dplyr)


# Data -----------------------------------------------------------------

paut23 <- readr::read_delim("data/Paut23/1. Database.csv", delim = ";")

corinecountries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Iceland", "Liechtenstein", "Norway", "Switzerland", "Albania", "Bosnia and Herzegovina", "Kosovo", "Montenegro", "North Macedonia", "Serbia", "Turkey")

paut23_eu <- paut23 %>% filter(Country %in% corinecountries)

glimpse(paut23_eu)

paut23_eu_af <- 
paut23_eu %>% 
  filter(AF == "yes")


# Exploration  ------------------------------------------------------------


sink("results/paut23_exploration_20260612.txt")

cat("#########################################")
cat("\n")
cat("Paut 2023, Exploration")
cat("\n")
cat("Date: 12.06.2026")
cat("\n")
cat("#########################################")
cat("\n")

cat("Years\n\n")

print(paut23_eu_af %>% count(Experiment_year))

cat("\n")

cat("Countries\n\n")

print(paut23_eu_af %>% count(Id_article, Country))

cat("\n")

cat("Crop and Tree\n\n")

print(paut23_eu_af %>% count(Crop_1_Common_Name, Crop_2_Common_Name))

cat("\n")

print(paut23_eu_af %>% count(Yield_unit))

cat("\n")

cat("Yield Measurements\n\n")

paut23_eu_af %>% count(Id_article, Yield_unit, Yield_measure, Yield_total_intercropping_calc)

cat("\n")

cat("Yield Crop and Tree\n\n")

paut23_eu_af %>% count(Crop_1_Common_Name, Yield_unit, C1_yield_sole, Crop_2_Common_Name, C2_yield_sole)

cat("\n")


#close the external connection
sink() 


# Save the csv ------------------------------------------------------------

write.csv(paut23_eu_af, "data/Paut23/20260612_paut23_euaf.csv", row.names = FALSE)
write.csv(paut23_eu_af, "data/collection/20260612_paut23_euaf.csv", row.names = FALSE)


  