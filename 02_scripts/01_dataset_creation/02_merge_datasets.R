# =============================================================================
# Step 2 — Merge all field datasets into one harmonised analysis dataset
# Input:  01_Data\\AnalysisData/fields_*.csv  (from Step 1)
#         01_Data\\AnalysisData/fieldpolygons.csv
#         01_Data\\AnalysisData/DataDescription_Fertilization.csv
# Output: 01_Data\\AnalysisData/AF_merged.csv
# =============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(janitor)

ANALYSIS_DIR <- "C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond\\01_Data\\AnalysisData"

# =============================================================================
# (1) Load per-site CSVs
# =============================================================================

datasets <- list(
  wendhausen    = read_csv(file.path(ANALYSIS_DIR, "fields_wendhausen.csv")),
  dornburg      = read_csv(file.path(ANALYSIS_DIR, "fields_dornburg.csv")),
  vechta        = read_csv(file.path(ANALYSIS_DIR, "fields_vechta.csv")),
  reiffenhausen = read_csv(file.path(ANALYSIS_DIR, "fields_reiffenhausen.csv")),
  mariensee     = read_csv(file.path(ANALYSIS_DIR, "fields_mariensee.csv")),
  forst         = read_csv(file.path(ANALYSIS_DIR, "fields_forst.csv")),
  gladbacherhof = read_csv(file.path(ANALYSIS_DIR, "fields_gladbacherhof.csv")),
  koch          = read_csv(file.path(ANALYSIS_DIR, "fields_koch_1623.csv"))
)

message("Row counts per dataset:")
print(sapply(datasets, nrow))

# =============================================================================
# (2) Column audit: check required columns are present and correctly typed
# =============================================================================

required_cols <- c(
  yield_unit             = "character",
  yield                  = "numeric",
  lat                    = "numeric",
  lon                    = "numeric",
  data_id                = "character",
  field                  = "character",
  tree_species           = "character",
  distance_to_tree_strip = "numeric",
  year                   = "numeric",
  plot                   = "character",
  crop                   = "character"
)

check_cols <- function(df, name) {
  issues <- character()
  for (col in names(required_cols)) {
    if (!col %in% names(df)) {
      issues <- c(issues, paste0(col, ": MISSING"))
    } else if (!inherits(df[[col]], required_cols[[col]])) {
      issues <- c(issues, paste0(col, ": wrong type (", class(df[[col]]), ")"))
    }
  }
  if (length(issues)) {
    message("  ", name, ": ", paste(issues, collapse = "; "))
  } else {
    message("  ", name, ": OK")
  }
}

message("\nColumn audit:")
for (nm in names(datasets)) check_cols(datasets[[nm]], nm)

# =============================================================================
# (3) Bind all fields
# =============================================================================

all_fields <- bind_rows(datasets)
message("\nBound dataset: ", nrow(all_fields), " rows")

# =============================================================================
# (4) Standardise crop names
# =============================================================================

crop_lookup <- c(
  "sillage maize"       = "maize",
  "silage maize"        = "maize",
  "winterwheat"         = "wheat",
  "ww"                  = "wheat",
  "winter wheat"        = "wheat",
  "wb"                  = "barley",
  "wp"                  = "pea",
  "winter barley"       = "barley",
  "spring barley"       = "barley",
  "summer barley"       = "barley",
  "wr"                  = "rapeseed",
  "winter rye"          = "rye",
  "oil rape"            = "rapeseed",
  "winter oilseedrape"  = "rapeseed",
  "winter rapeseed"     = "rapeseed",
  "bristle oat"         = "oat"
)

all_fields <- all_fields %>%
  mutate(
    crop         = crop %>% str_replace_all("[-_]", " ") %>%
                            str_to_lower() %>% str_squish(),
    tree_species = tree_species %>% str_replace_all("[-_]", " ") %>%
                                    str_to_lower() %>% str_squish(),
    crop_unified = if_else(crop %in% names(crop_lookup),
                           crop_lookup[crop], crop)
  )

message("Crop levels after unification:")
print(sort(unique(all_fields$crop_unified)))

# =============================================================================
# (5) Convert yield to t/ha
# =============================================================================

all_fields <- all_fields %>%
  mutate(
    unit_factor = case_when(
      yield_unit == "t/ha"                          ~ 1,
      yield_unit == "kg/m2"                         ~ 10,
      yield_unit %in% c("g/m2", "g/m2 per year")   ~ 0.01,
      TRUE                                          ~ NA_real_
    ),
    yield_tha       = yield       * unit_factor,
    yield_straw_tha = yield_straw * unit_factor
  ) %>%
  select(-unit_factor)

# =============================================================================
# (6) Add tree age and planting year
# =============================================================================

all_fields <- all_fields %>%
  mutate(
    harvestyear = case_when(
      field == "Dornburg"                          ~ 2014,
      field == "Forst"                             ~ 2014,
      field == "Gladbacherhof"                     ~ 2019,
      field == "IhingerHof" & year < 2019          ~ 2016,
      field == "IhingerHof" & year >= 2022         ~ 2022,
      field == "IhingerHof" & year >= 2019         ~ 2019,
      field == "Mariensee"                         ~ 2015,
      field == "Reiffenhausen"                     ~ 2015,
      field == "Vechta"                            ~ 2018,
      field == "Wendhausen" & year < 2021          ~ 2013,
      field == "Wendhausen" & year >= 2021         ~ 2021,
      TRUE                                         ~ NA_real_
    ),
    treeage = year - harvestyear
  )

# =============================================================================
# (7) Remove control plots (no distance to tree strip)
# =============================================================================

n_before <- nrow(all_fields)
all_fields <- filter(all_fields, !is.na(distance_to_tree_strip))
all_fields <- filter(all_fields, !is.na(yield))

# there are also some typos in Vechta and Forst dataset that were identified visually 
# remove those 

all_fields <-  all_fields |> 
 filter(!(year == 2020 & plot == "V_AF_r4_4m"))

all_fields <-  all_fields |> 
 filter(!(year == 2021 & plot == "V_AF_r1_18m"))

# 1 correction to Forst
all_fields <- all_fields %>%
  mutate(
    plot = if_else(
      plot == "F_AF_r3_24m" & yield == 625.42,
      "F_AF_r4_24m",
      plot
    )
  )


message("Removed ", n_before - nrow(all_fields),
        " control rows (no distance_to_tree_strip); ", nrow(all_fields), " remain")

# Drop two NA-yield rows
all_fields <- filter(all_fields, !is.na(yield_tha))

# =============================================================================
# (8) Join field bounding-box coordinates from fieldpolygons.csv
# =============================================================================
# fieldpolygons.csv uses long names ("Ihinger Hof Field" etc.); map to the
# short names used in the yield dataset.

field_name_map <- c(
  "Ihinger Hof Field"    = "IhingerHof",
  "Dornburg field"       = "Dornburg",
  "Forst Field"          = "Forst",
  "Gladbacherhof Field"  = "Gladbacherhof",
  "Mariensee Field"      = "Mariensee",
  "Reiffenhausen Field"  = "Reiffenhausen",
  "Vechta Field"         = "Vechta",
  "Wendhausen field"     = "Wendhausen"
)

fieldpolygons <- read_csv(file.path(ANALYSIS_DIR, "fieldpolygons.csv")) %>%
  mutate(field = recode(Name, !!!field_name_map)) %>%
  select(field, Latitude, Longitude,
         minLatitude, minLongitude, maxLatitude, maxLongitude, Area) %>%
  distinct()

all_fields <- all_fields %>%
  left_join(fieldpolygons, by = "field")

# =============================================================================
# (9) Build unique observation ID
# =============================================================================

all_fields <- all_fields %>%
  mutate(
    id = case_when(
      str_detect(plot, "m$")    ~ paste0(plot, "_", year),
      str_detect(plot, "^Ih")   ~ paste0(plot, "m_", year),
      str_detect(plot, "^[A-Z]") ~ paste0(plot, "_", distance_to_tree_strip, "m_", year),
      field == "Gladbacherhof"  ~ paste0(plot, "_", distance_to_tree_strip, "m_", year),
      TRUE                      ~ NA_character_
    )
  )

n_dup <- sum(duplicated(all_fields$id) | duplicated(all_fields$id, fromLast = TRUE))
if (n_dup > 0) {
  message("Removing ", n_dup, " rows with duplicated IDs")
  all_fields <- all_fields %>%
    filter(!(duplicated(id) | duplicated(id, fromLast = TRUE)))
}

# =============================================================================
# (10) Join fertilisation data and add growing-season dates
# =============================================================================

fertilization <- read_csv(file.path(ANALYSIS_DIR, "DataDescription_Fertilization.csv")) %>%
  janitor::clean_names()

all_fields$fertilizer[is.na(all_fields$fertilizer)] <- "normal"

all_fields <- all_fields %>%
  left_join(fertilization,
            by = c("field", "year" = "harvest_year",
                   "crop_unified", "fertilizer")) %>%
  mutate(
    growing_start  = match(sowing_month,  month.abb),
    growing_end    = match(harvest_month, month.abb),
    gs_start_year  = if_else(growing_start >= 7, year - 1L, year),
    gs_start_date  = as.Date(sprintf("%04d-%02d-01", gs_start_year, growing_start)),
    gs_end_date    = as.Date(sprintf("%04d-%02d-01", year,           growing_end))
  ) %>%
  select(-gs_start_year)

# =============================================================================
# (11) Final column order and save
# =============================================================================

all_fields <- all_fields %>%
  janitor::clean_names()

write_csv(all_fields, file.path(ANALYSIS_DIR, "AF_merged.csv"))



message("\nStep 2 complete — AF_merged.csv  (", nrow(all_fields), " rows, ",
        ncol(all_fields), " columns)")
