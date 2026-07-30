rm(list=ls())


# Packages ----------------------------------------------------------------

library(tidyverse)


# Load datasets -------------------------------------------------------------

wendhausen    <- read_csv("data/AnalysisData/20260702_wendhausen.csv")
dornburg      <- read_csv("data/AnalysisData/20260701_dornburg.csv")
vechta        <- read_csv("data/AnalysisData/20260702_vechta.csv")
reiffenhausen <- read_csv("data/AnalysisData/20260702_reiffenhausen.csv")
mariensee     <- read_csv("data/AnalysisData/20260702_mariensee.csv")
forst         <- read_csv("data/AnalysisData/20260722_forst.csv")
gladbacherhof <- read_csv("data/AnalysisData/20260703_gladbacherhof.csv")
koch          <- read_csv("data/AnalysisData/20260705_koch1623.csv")

datasets <- list(
  wendhausen = wendhausen,
  dornburg = dornburg,
  vechta = vechta,
  reiffenhausen = reiffenhausen,
  mariensee = mariensee,
  forst = forst,
  gladbacherhof = gladbacherhof,
  koch = koch
)

# check nrow
sapply(datasets, nrow)


# check required columns -----------------------------------------------------

required_cols <- c(
  yield_unit = "character",
  yield = "numeric",
  lat = "numeric",
  lon = "numeric",
  data_id = "character",
  field = "character",
  tree_species = "character",
  distance_to_tree_strip = "numeric",
  year = "numeric",
  plot = "character",
  crop = "character"
)

check_cols <- function(df, name) {
  cat("---", name, "---\n")
  for (col in names(required_cols)) {
    if (!col %in% names(df)) {
      cat(col, ": missing\n")
    } else if (!inherits(df[[col]], required_cols[[col]])) {
      cat(col, ": wrong type, is", class(df[[col]]), "\n")
    } else {
      cat(col, ": ok\n")
    }
  }
}

for (name in names(datasets)) {
  check_cols(datasets[[name]], name)
}

# (dornburg missing lat lon)
# (vechta missing lat lon)
# (reiffenhausen missing lat lon)
# (reiffenhausen missing plot)
# (mariensee missing lon)
# (forst missing lon)
# (forst missing tree species)
# (gladbacherhof missing tree species)
# (gladbacherhof distance_to_tree_strip : wrong type, is character) 
# (koch missing yield)
# (koch missing distance to tree strip)

# bind rows -------------------------------------------------------------

all_fields <- bind_rows(datasets)

# check
nrow(all_fields) #1484
colSums(is.na(all_fields))


# clean -------------------------------------------------------------------

all_fields <- all_fields %>%
  mutate(
    crop = crop %>% str_replace_all("[-_]", " ") %>% str_to_lower() %>% str_squish(),
    tree_species = tree_species %>% str_replace_all("[-_]", " ") %>% str_to_lower() %>% str_squish()
  )

levels(as.factor(all_fields$crop))
levels(as.factor(all_fields$yield_unit))


crop_lookup <- c(
  "sillage maize" = "maize",
  "silage maize" = "maize",
  "winterwheat" = "wheat",
  "ww" = "wheat",
  "winter wheat" = "wheat",
  "wb" = "barley",
  "wp" = "pea",
  "winter barley" = "barley",
  "spring barley" = "barley",
  "summer barley" = "barley",
  "wr" = "rye",
  "winter rye" = "rye",
  "oil rape" = "rapeseed",
  "winter oilseedrape" = "rapeseed",
  "winter rapeseed" = "rapeseed",
  "bristle oat" = "oat"
)

all_fields <- all_fields %>%
  mutate(crop_unified = if_else(crop %in% names(crop_lookup),
                                crop_lookup[crop],
                                crop))

# convert yield, straw and total to t/ha
conversion <- case_when(
  all_fields$yield_unit == "t/ha" ~ 1,
  all_fields$yield_unit == "kg/m2" ~ 10,
  all_fields$yield_unit %in% c("g/m2", "g/m2 per year") ~ 0.01,
  TRUE ~ NA_real_
)

all_fields <- all_fields %>%
  mutate(
    yield_tha = yield * conversion,
    yield_straw_tha = yield_straw * conversion,
    yield_total_tha = yield_total * conversion
  )

# write 
write.csv(all_fields, file = "data/AnalysisData/20260722_all_fields.csv", row.names = FALSE)



# Clean the merged dataset ------------------------------------------------
rm(list=ls())
df <- read.csv("data/AnalysisData/20260722_all_fields.csv")
str(df)

# (1) Vechta has a typo in the year 2020
dfna <- df[df$field == "Vechta" & df$year == 2020 & grepl("_4m$", df$plot), ]
df <- df[!rownames(df) %in% rownames(dfna), ]
# check
dfna <- df[df$field == "Vechta" & df$year == 2020 & grepl("_4m$", df$plot), ]
dfna

# (2) for Forst changed value manually

# (3) Dornburg na kick out 
# before 1480 obs
df <- df[!is.na(df$yield), ]
# now 1478

# (4) kick out Gladbacherhof 2022 because they have a labelling problem
dfna <- df[df$field == "Gladbacherhof" & df$year == 2022, ]
df <- df[!rownames(df) %in% rownames(dfna), ]
# now 1328 

write.csv(df, file = "data/AnalysisData/20260723_all_fields.csv", row.names = FALSE)

# rework further 26/7/2026

df <-  read.csv("data/AnalysisData/20260723_all_fields.csv")
