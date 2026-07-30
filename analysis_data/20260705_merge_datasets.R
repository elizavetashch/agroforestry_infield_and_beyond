rm(list=ls())


# Packages ----------------------------------------------------------------

library(tidyverse)


# Load datasets -------------------------------------------------------------

wendhausen    <- read_csv("data/AnalysisData/20260702_wendhausen.csv")
dornburg      <- read_csv("data/AnalysisData/20260701_dornburg.csv")
vechta        <- read_csv("data/AnalysisData/20260702_vechta.csv")
reiffenhausen <- read_csv("data/AnalysisData/20260702_reiffenhausen.csv")
mariensee     <- read_csv("data/AnalysisData/20260702_mariensee.csv")
forst         <- read_csv("data/AnalysisData/20260702_forst.csv")
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
  check_cols(datasets[[name]], name) # all updated and currently no missing.
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


# exlude control treatments
z <- all_fields[is.na(all_fields$distance_to_tree_strip), ] # visually inspected, those are only controls
nrow(all_fields)# 1484
nrow(z) # 148
all_fields_nocontrol <- all_fields[!is.na(all_fields$distance_to_tree_strip), ]
nrow(all_fields_nocontrol)# 1336

all_fields_nocontrol$row_id <- paste(
  substr(all_fields_nocontrol$field, 1, 1),
  all_fields_nocontrol$distance_to_tree_strip,
  all_fields_nocontrol$year,
  sep = "_"
)

# Extract plot information from three different formats and create row_id
# Extract plot information from three different formats and create row_id

# Extract plot information from three different formats and create row_id

# Extract plot information from three different formats and create row_id

# Function to extract plot information based on format
extract_plot_info <- function(plot) {
  
  # Type 1: W_AF_r1_1m or M-AF-r5 - extract just "r" + number
  if (grepl("[rR]\\d+", plot)) {
    return(regmatches(plot, regexpr("[rR]\\d+", plot)))
  }
  
  # Type 2: AF_GH1_Yield_4c-i - extract ONLY what comes after "Yield_"
  if (grepl("Yield_", plot)) {
    return(sub(".*Yield_(.+)$", "\\1", plot))
  }
  
  # Type 2b: AF_GH1_INF_1b-i or AF_GH1_RKS_1b-I - extract what comes after the last underscore
  if (grepl("(INF|RKS)_", plot)) {
    return(sub(".*_(.+)$", "\\1", plot))
  }
  
  # Type 3: 22_4_18 - extract first part before last underscore (22_4)
  if (grepl("^\\d+_\\d+_\\d+$", plot)) {
    return(sub("(.+)_\\d+$", "\\1", plot))
  }
  
  # Return original if no pattern matches
  return(plot)
}

# Create the extraction
all_fields_nocontrol$plot_extraction <- sapply(all_fields_nocontrol$plot, extract_plot_info)

# For Type 3 entries (22_4_18 format), add sequential suffix for duplicates within each plot group
type3_indices <- grepl("^\\d+_\\d+_\\d+$", all_fields_nocontrol$plot)

# Create a function to convert numbers to roman numerals
num_to_roman <- function(num) {
  numerals <- c("i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x",
                "xi", "xii", "xiii", "xiv", "xv", "xvi", "xvii", "xviii", "xix", "xx")
  if (num > length(numerals)) {
    return(as.character(num))
  }
  return(numerals[num])
}

# Add suffixes to Type 3, resetting within each unique plot value
if (any(type3_indices)) {
  type3_data <- all_fields_nocontrol[type3_indices, ]
  
  # For each unique plot value, add sequential suffixes
  for (plot_val in unique(type3_data$plot)) {
    idx <- which(all_fields_nocontrol$plot == plot_val & type3_indices)
    
    # Only add suffixes if there are duplicates (more than 1 occurrence)
    if (length(idx) > 1) {
      all_fields_nocontrol$plot_extraction[idx] <- paste0(
        all_fields_nocontrol$plot_extraction[idx],
        "_",
        sapply(seq_along(idx), num_to_roman)
      )
    }
  }
}

# Now create the final row_id
all_fields_nocontrol$row_id <- paste(
  substr(all_fields_nocontrol$field, 1, 1),
  all_fields_nocontrol$distance_to_tree_strip,
  all_fields_nocontrol$year,
  all_fields_nocontrol$plot_extraction,
  sep = "_"
)

# View the results
all_fields_nocontrol[all_fields_nocontrol$field=="IhingerHof", ] %>% select(plot, plot_extraction, row_id)
all_fields_nocontrol[all_fields_nocontrol$field=="Gladbacherhof", ] %>% select(plot, plot_extraction, row_id)
all_fields_nocontrol[all_fields_nocontrol$field=="Wendhausen", ] %>% select(plot, plot_extraction, row_id)
all_fields_nocontrol[all_fields_nocontrol$field=="Dornburg", ] %>% select(plot, plot_extraction, row_id)

# still some of them are true 
z <- all_fields_nocontrol[duplicated(all_fields_nocontrol$row_id)==TRUE, ]
z <- all_fields_nocontrol[all_fields_nocontrol$row_id=="F_24_2017_r3", ]

# Check if duplicated() is case-sensitive
duplicated(c("1b-i", "1b-I"))  # Should return FALSE TRUE if case-sensitive

# Look at all duplicates for that specific row_id pattern
z <- all_fields_nocontrol[duplicated(all_fields_nocontrol$row_id, fromLast = TRUE) | 
                            duplicated(all_fields_nocontrol$row_id), ]

# Filter more carefully - show EXACT duplicate row_ids (case-sensitive)
gh <- z[z$field == "Gladbacherhof", ] %>% 
  group_by(sample_name_db) %>% 
  filter(n() > 1) %>% 
  arrange(sample_name_db)

print(z_exact)


# write 
write.csv(all_fields, file = "data/AnalysisData/20260705_all_fields.csv", row.names = FALSE)
