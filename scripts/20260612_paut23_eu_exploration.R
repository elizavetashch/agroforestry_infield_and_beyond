# ============================================================
# Data Exploration: paut23_eu
# Author: Elizaveta
# Date: 2026-06-12
# ============================================================

library(tidyverse)

# ---- 1. Overview -------------------------------------------------------

dim(paut23_eu)
glimpse(paut23_eu)

# Count of unique articles vs observations
paut23_eu |>
  summarise(
    n_articles = n_distinct(Id_article),
    n_obs      = n()
  )

# ---- 2. Missing Data ---------------------------------------------------

# Proportion of NAs per column
paut23_eu |>
  summarise(across(everything(), ~ mean(is.na(.)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "prop_missing") |>
  filter(prop_missing > 0) |>
  arrange(desc(prop_missing)) |>
  print(n = Inf)

# Visualise missingness
paut23_eu |>
  summarise(across(everything(), ~ mean(is.na(.)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "prop_missing") |>
  filter(prop_missing > 0) |>
  ggplot(aes(x = prop_missing, y = reorder(column, prop_missing))) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Proportion of missing values per column",
       x = "% missing", y = NULL)

# ---- 3. Publication & Temporal Coverage --------------------------------

# Publications per year
paut23_eu |>
  count(Year_of_publication) |>
  ggplot(aes(x = Year_of_publication, y = n)) +
  geom_col() +
  labs(title = "Observations by publication year",
       x = "Year", y = "Number of observations")

# Observations per article (how many obs does each paper contribute?)
paut23_eu |>
  count(Id_article, sort = TRUE) |>
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Observations per article",
       x = "Observations", y = "Number of articles")

# ---- 4. Geographic Distribution ----------------------------------------

paut23_eu |>
  count(Country, sort = TRUE) |>
  print(n = Inf)

paut23_eu |>
  count(Country, sort = TRUE) |>
  ggplot(aes(x = n, y = reorder(Country, n))) +
  geom_col() +
  labs(title = "Observations by country",
       x = "Number of observations", y = NULL)

paut23_eu |>
  count(Climate_zone, sort = TRUE)

# ---- 5. Experimental Design Variables ---------------------------------

# Intercropping design (replacement vs. additive)
paut23_eu |> count(Intercropping_design, sort = TRUE)

# Pattern
paut23_eu |> count(Intercropping_pattern, sort = TRUE)

# Agroforestry flag
paut23_eu |> count(AF, sort = TRUE)

# Greenhouse vs. field
paut23_eu |> count(Greenhouse, sort = TRUE)

# Fertilisation types
paut23_eu |>
  count(Organic_ferti, Mineral_ferti) |>
  arrange(desc(n))

# Pesticide use
paut23_eu |>
  count(Herbicide, Insecticide, Fungicide)

# Irrigation
paut23_eu |> count(Irrigation, sort = TRUE)

# ---- 6. Crop Combinations ----------------------------------------------

# Most frequent crop 1
paut23_eu |> count(Crop_1_Common_Name, sort = TRUE) |> print(n = 20)

# Most frequent crop 2
paut23_eu |> count(Crop_2_Common_Name, sort = TRUE) |> print(n = 20)

# Most frequent crop pairs
paut23_eu |>
  mutate(crop_pair = paste(Crop_1_Common_Name, "+", Crop_2_Common_Name)) |>
  count(crop_pair, sort = TRUE) |>
  print(n = 20)

# Combination type: duration, growth form, N-fixation, metabolism
paut23_eu |> count(Comb_dur, sort = TRUE)
paut23_eu |> count(Comb_gf,  sort = TRUE)
paut23_eu |> count(Comb_Nf,  sort = TRUE)
paut23_eu |> count(Comb_met, sort = TRUE)

# ---- 7. Yield Variables ------------------------------------------------

# Summary statistics for key yield columns
paut23_eu |>
  select(C1_yield_sole, C1_yield_intercrop,
         C2_yield_sole, C2_yield_intercropped,
         Yield_total_intercropping_calc) |>
  summary()

# Yield units used
paut23_eu |> count(Yield_unit, sort = TRUE)

# Yield measure types
paut23_eu |> count(Yield_measure, sort = TRUE)

# ---- 8. LER Variables --------------------------------------------------

# Summary statistics
paut23_eu |>
  select(LER_crop1, LER_crop2, LER_tot,
         LER_crop_1_calc, LER_crop_2_calc, LER_tot_calc) |>
  summary()

# Distribution of total LER
paut23_eu |>
  # Use reported LER_tot where available, fall back to calculated
  mutate(LER_total = coalesce(LER_tot, LER_tot_calc)) |>
  filter(!is.na(LER_total)) |>
  ggplot(aes(x = LER_total)) +
  geom_histogram(binwidth = 0.1) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "red") +
  labs(title = "Distribution of total LER",
       subtitle = "Dashed line at LER = 1 (break-even)",
       x = "LER total", y = "Count")

# Proportion of observations with LER > 1
paut23_eu |>
  mutate(LER_total = coalesce(LER_tot, LER_tot_calc)) |>
  filter(!is.na(LER_total)) |>
  summarise(
    n              = n(),
    prop_above_1   = mean(LER_total > 1),
    median_LER     = median(LER_total),
    mean_LER       = mean(LER_total)
  )

# LER by intercropping design
paut23_eu |>
  mutate(LER_total = coalesce(LER_tot, LER_tot_calc)) |>
  filter(!is.na(LER_total), !is.na(Intercropping_design)) |>
  ggplot(aes(x = LER_total, y = Intercropping_design)) +
  geom_boxplot() +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "red") +
  labs(title = "LER by intercropping design",
       x = "LER total", y = NULL)

# LER by N-fixation combination
paut23_eu |>
  mutate(LER_total = coalesce(LER_tot, LER_tot_calc)) |>
  filter(!is.na(LER_total), !is.na(Comb_Nf)) |>
  ggplot(aes(x = LER_total, y = Comb_Nf)) +
  geom_boxplot() +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "red") +
  labs(title = "LER by N-fixation combination",
       x = "LER total", y = "N-fixation (crop1-crop2)")

# ---- 9. Crop 1 Yield Ratio (intercrop vs. sole) -----------------------

paut23_eu |>
  filter(!is.na(C1_yield_sole), !is.na(C1_yield_intercrop),
         C1_yield_sole > 0) |>
  mutate(C1_yield_ratio = C1_yield_intercrop / C1_yield_sole) |>
  summarise(
    n           = n(),
    median      = median(C1_yield_ratio),
    mean        = mean(C1_yield_ratio),
    prop_above1 = mean(C1_yield_ratio > 1)
  )
