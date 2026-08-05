rm(list=ls())


# Packages ----------------------------------------------------------------

library(tidyverse)
library(dplyr)

# Load datasets -------------------------------------------------------------

wendhausen    <- read_csv("data/AnalysisData/20260702_wendhausen.csv")
dornburg      <- read_csv("data/AnalysisData/20260701_dornburg.csv")
vechta        <- read_csv("data/AnalysisData/20260702_vechta.csv")
reiffenhausen <- read_csv("data/AnalysisData/20260702_reiffenhausen.csv")
mariensee     <- read_csv("data/AnalysisData/20260702_mariensee.csv")
forst         <- read_csv("data/AnalysisData/20260722_forst.csv")
gladbacherhof <- read_csv("data/AnalysisData/20260730_gladbacherhof.csv")
koch          <- read_csv("data/AnalysisData/20260730_koch1623.csv")

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
nrow(all_fields) #1316
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
  "wr" = "rapeseed",
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
    yield_straw_tha = yield_straw * conversion
  )

# write 
# write.csv(all_fields, file = "data/AnalysisData/20260722_all_fields.csv", row.names = FALSE)



# Clean the merged dataset ------------------------------------------------

df <- all_fields
str(df)

# # (1) Vechta has a typo in the year 2020
# dfna <- df[df$field == "Vechta" & df$year == 2020 & grepl("_4m$", df$plot), ]
# df <- df[!rownames(df) %in% rownames(dfna), ]
# # check
# dfna <- df[df$field == "Vechta" & df$year == 2020 & grepl("_4m$", df$plot), ]
# dfna

# (2) for Forst changed value manually

# (3) Dornburg na kick out 
# before 1480 obs
df <- df[!is.na(df$yield), ]
# now 1478

write.csv(df, file = "data/AnalysisData/20260730_all_fields.csv", row.names = FALSE)


# Additional Variabels  ---------------------------------------------------

# additional data preparation 
df <- df |> mutate( harvestyear = case_when( 
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


# rework further 30/7/2026
df <- df[!is.na(df$distance_to_tree_strip), ] # delete control treatments
write.csv(df, file = "data/AnalysisData/20260731_AF_fields.csv", row.names = FALSE)

colnames(df)
nrow(df) #1174

# add real shape sizes, latitude and longitude 

rm(list=ls())
df <- read.csv("data/AnalysisData/20260731_AF_fields.csv")

# add shape lat longs 
swf <- read_csv("analysis_data/20260726_propswf.csv")

dfjoin <- swf[,c(1, 9:15)] |> unique()
dffull <- df %>% left_join(dfjoin, by = "field")

dfanalysis <-   dffull |> 
  select(data_id, plot, field, year, distance_to_tree_strip, crop_unified, yield_tha, yield_straw_tha,
         tree_species, fertilizer, harvestyear, treeage, 
         Latitude, Longitude, minLatitude, minLongitude, maxLatitude, maxLongitude, Area )
dfanalysis <- janitor::clean_names(dfanalysis)  
colnames(dfanalysis)
str(dfanalysis)
summary(dfanalysis)


# id introduced
dfanalysis <- dfanalysis %>%
  mutate(
    id = case_when(
      
      # plot ends with "m"
      str_detect(plot, "m$") ~ paste0(plot, "_", year),
      
      # plot starts with "Ih"
      str_detect(plot, "^Ih") ~ paste0(plot, "m_", year),
      
      # plot starts with a capital letter
      str_detect(plot, "^[A-Z]") ~ paste0(
        plot, "_", distance_to_tree_strip, "m_", year
      ),
      
      # fallback for Gladbacherhof
      field == "Gladbacherhof" ~ paste0(
        plot, "_", distance_to_tree_strip, "m_", year
      ),
      
      TRUE ~ NA_character_
    )
  )

dfanalysis[duplicated(dfanalysis$id) | duplicated(dfanalysis$id, fromLast = TRUE), ]
# remove all duplicated ids
dfanalysis <- dfanalysis[!(duplicated(dfanalysis$id) | duplicated(dfanalysis$id, fromLast = TRUE)), ]
nrow(dfanalysis) # 1171

# just reorder the id to the front 
dfanalysis <- dfanalysis |> 
  select(id, data_id, plot, field, year, distance_to_tree_strip, crop_unified, yield_tha, yield_straw_tha,
         tree_species, fertilizer, harvestyear, treeage, 
         latitude, longitude, min_latitude, min_longitude, max_latitude, max_longitude, area )

# add fertilization data
fertilization <- read_csv("data/DataDescription_Fertilization.csv")
fertilization <- janitor::clean_names(fertilization)
dfanalysis$fertilizer[is.na(dfanalysis$fertilizer)] <- "normal"
dffull <- dfanalysis %>% left_join(fertilization, by = c("field", "year"="harvest_year", "crop_unified", "fertilizer"))

# growing season
dffull$growing_start <- match(dffull$sowing_month, month.abb)
dffull$growing_end   <- match(dffull$harvest_month, month.abb)


# growing start
dffull$gs_start_year <- ifelse(dffull$growing_start >= 7,
                               dffull$year - 1,
                               dffull$year)
dffull$gs_start_date <- as.Date(
  sprintf("%04d-%02d-01",
          dffull$gs_start_year,
          dffull$growing_start)
)

# grwing end
dffull$gs_end_date <- as.Date(
  sprintf("%04d-%02d-01",
          dffull$year,
          dffull$growing_end)
)

# check result
head(dffull[, c("year", "growing_start", "gs_start_date", "gs_end_date")])
dffull$gs_start_year <- NULL

dfanalysis <- dffull
write.csv(dfanalysis, file = "data/AnalysisData/20260801_dfanalysis.csv", row.names = FALSE)

str(dfanalysis)
