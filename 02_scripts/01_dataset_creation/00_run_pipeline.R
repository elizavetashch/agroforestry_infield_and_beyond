
# file creation date: 09/09/2026
# author: Elizaveta Shcherbinina 
# master thesis

# This piplene was created to optimize the codes that piled up throughout the 
# MA_Project. 

# This pipeline was created by auditing the code created in the MA_Project 
# through Claude. After auditing with Claude all of the adjusted files were 
# checked and run. The final datset was compare to the one produced through the
# MA_Project. 

# =============================================================================
# Dataset Creation
# =============================================================================
# Run this script to execute the full data preparation pipeline in order.
# Each step reads from the previous step's output in 01_Data/AnalysisData/.
# =============================================================================

# Set to FALSE once a step's output already exists and is up to date
RUN_SINGLE_FIELDS    <- TRUE   # Step 1: ~2  min — reads raw CSVs
RUN_MERGE_DATASETS  <- TRUE   # Step 2: < 1  min — binds + harmonises
RUN_CLIMATE         <- TRUE   # Step 3: ~10 min — DWD grid download
RUN_LULC_RASTERS    <- TRUE   # Step 4a: ~5  min — clips + saves TIFs (one-time)
RUN_LULC_METRICS    <- TRUE   # Step 4b: ~5  min — landscape metrics
RUN_LULC_MERGE      <- TRUE   # Step 4c: <1  min — joins metrics to dataset
RUN_SLOPE           <- TRUE   # Step 5: <1  min
RUN_SWF_RASTERS     <- TRUE   # Step 6a: ~10 min — annulus extraction (one-time)
RUN_SWF_MERGE       <- TRUE   # Step 6b: <1  min
RUN_SOILTEXTURE     <- TRUE   # Step 7: <1  min

# ---- Shared paths (edit here if your project root differs) -----------------
ANALYSIS_DIR <- "C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond\\01_Data\\AnalysisData"
dir.create(ANALYSIS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create("01_Data\\AnalysisData\\climate/rdwd",  recursive = TRUE, showWarnings = FALSE)
dir.create("01_Data\\AnalysisData\\LULC",        recursive = TRUE, showWarnings = FALSE)
dir.create("01_Data\\AnalysisData\\swf",         recursive = TRUE, showWarnings = FALSE)

# ---- Run steps -------------------------------------------------------------

# The logic of the steps is the following that the consequent step 
# is always aggregated to the dataset produced in the previous step. 
# Hence, if only one step has been updated - silence everything that comes before
# by setting the steps to FALSE and rerun only the up-following part of the analysis. 

if (RUN_SINGLE_FIELDS)   source("02_scripts/01_dataset_creation/01_single_fields.R",    local = TRUE)
setwd("C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond")
if (RUN_MERGE_DATASETS) source("02_scripts/01_dataset_creation/02_merge_datasets.R",  local = TRUE)
setwd("C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond")
if (RUN_CLIMATE)        source("02_scripts/01_dataset_creation/03_climate.R",         local = TRUE)
setwd("C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond")
if (any(RUN_LULC_RASTERS, RUN_LULC_METRICS, RUN_LULC_MERGE))
                        source("02_scripts/01_dataset_creation/04_lulc.R",            local = TRUE)
setwd("C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond")
if (RUN_SLOPE)          source("02_scripts/01_dataset_creation/05_slope.R",           local = TRUE)
setwd("C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond")
if (any(RUN_SWF_RASTERS, RUN_SWF_MERGE))
                        source("02_scripts/01_dataset_creation/06_swf.R",             local = TRUE)
setwd("C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond")
if (RUN_SOILTEXTURE)    source("02_scripts/01_dataset_creation/07_soiltexture.R",     local = TRUE)

setwd("C:\\Users\\Elizaveta\\OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RGit_agroforestry_infield_and_beyond")

source("02_scripts/01_dataset_creation/08_finalmerge.R",     local = TRUE)

writeLines(paste("Last update:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),file.path(ANALYSIS_DIR, "last_update.txt"))


message("Pipeline complete. Final dataset: 01_Data/dffinal_20260909.csv")
