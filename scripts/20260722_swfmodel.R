rm(list=ls())

# packages
libraries <- c("raster", "sp", "sf", "dplyr", "tidyr", "ggplot2", 
               "mgcv", "refund", "terra", "data.table", "parallel")

for (lib in libraries) {
    library(lib, character.only = TRUE)
}

# rasters and polygons 
# load raster 
tif_files <- list.files(".\\data\\SWF\\2021", pattern = "\\.tif$", full.names = TRUE)
r <- terra::rast(tif_files)
crs(r)

# reproject 
fieldpolygons_proj <- fieldpolygons %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(r))

coords <- st_coordinates(fieldpolygons_proj)







# 1.1 Load woody features raster (5m resolution)
tif_files <- list.files(".\\data\\SWF\\2021", pattern = "\\.tif$", full.names = TRUE)
woody_map <- terra::rast(tif_files)

# 1.2 Load field coordinates dataset
fields <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")

# Expected columns: Name, x, y, municipal_district, yield (optional)
# Optional additional columns: field_area, crop_type, etc.

# Convert to sf object for spatial operations
fields_sf <- st_as_sf(fields, coords = c("Longitude", "Latitude"), crs = st_crs(woody_map))

# ============================================================================
# PART 2: EXTRACT NON-CROP LAND COVER PROFILES
# ============================================================================

# 2.1 Define annulus radii for sampling (distance classes)
# Note: Adjust max_radius based on your landscape scale
max_radius <- 500  # meters
radius_step <- 50  # meters (creates annuli of 50m width)
annulus_radii <- seq(radius_step, max_radius, by = radius_step)




#
extract_annulus_profile <- function(point_coords, raster, radii, step) {
  # Extracts proportion of non-crop land (woody features) at each annulus (ring)
  # Creates true annulus by masking out inner circle from outer circle
  
  results <- data.frame(radius = numeric(), prop_noncrop = numeric())
  
  for (r in radii) {
    # Create buffer at inner and outer radius
    r_inner <- r - step
    r_outer <- r
    
    # Create outer buffer
    buffer_outer <- st_buffer(point_coords, dist = r_outer)
    
    # Create inner buffer (to be masked out)
    buffer_inner <- st_buffer(point_coords, dist = r_inner)
    
    # Create annulus mask: outer circle minus inner circle
    # This creates a true ring-shaped area
    annulus_mask <- st_difference(buffer_outer, buffer_inner)
    
    # Extract values only in the annulus
    vals_annulus <- extract(raster, annulus_mask)
    
    # Calculate proportion of non-crop (1s) in annulus
    # vals_annulus[[1]] = cell IDs, vals_annulus[[2]] = actual raster values
    if (!is.null(vals_annulus) && length(vals_annulus[[2]]) > 0) {
      annulus_values <- vals_annulus[[2]]
      annulus_values <- annulus_values[!is.na(annulus_values)]
      
      if (length(annulus_values) > 0) {
        prop <- sum(annulus_values) / length(annulus_values)  # Proportion of 1s
      } else {
        prop <- NA
      }
    } else {
      prop <- NA
    }
    
    results <- rbind(results, data.frame(radius = r, prop_noncrop = prop))
  }
  
  return(results)
}

# 2.3 Apply to all fields (with parallelization for speed)
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)

# Export functions and objects to clusters
clusterExport(cl, c("woody_map", "annulus_radii", "radius_step", 
                    "extract_annulus_profile"), 
              envir = environment())
clusterEvalQ(cl, {
  library(terra)
  library(sf)
})

# Extract profiles for each field
field_profiles <- parLapply(seq_len(nrow(fields_sf)),
                            function(i) {
                              profile <- extract_annulus_profile(
                                st_geometry(fields_sf[i, ]),
                                woody_map,
                                annulus_radii,
                                radius_step
                              )
                              profile$Name <- fields_sf$Name[i]
                              return(profile)
                            }
)

stopCluster(cl)

# Combine results
profiles_df <- do.call(rbind, field_profiles)
rownames(profiles_df) <- NULL

print("Annulus profiles extracted")
print(head(profiles_df))


# data 
woody_map <- terra::rast(".\\output\\SWF\\2021\\Dornburg_field_buf1000m.tif")
crs(woody_map)
levels(woody_map)
fieldpolygons <- read.csv("data/ArcGIS_Outputs/fieldpolygons.csv")
fields_sf <- st_as_sf(fieldpolygons, coords = c("Longitude","Latitude"), crs = 4326) %>%
  st_transform(crs(woody_map))

max_radius <- 500
radius_step <- 50
annulus_radii <- seq(radius_step, max_radius, by = radius_step)


dcoord <- fieldpolygons %>% filter(Name == "Dornburg field") %>% select(Name, Latitude, Longitude)
dcoord <- st_as_sf(dcoord, coords = c("Longitude","Latitude"), crs = 4326) %>%
  st_transform(crs(woody_map))

woody_binary <- as.numeric(woody_map)

# sum of values 
r_inner <- 100 - 50 # alerady in meters
r_outer <- 100

buffer_outer <- st_buffer(dcoord, dist = r_outer)
buffer_inner <- st_buffer(dcoord, dist = r_inner)

vals_outer <- extract(woody_binary, buffer_outer) # pr2 = 1256, values = 1258, all good 
vals_inner <- extract(woody_binary, buffer_inner)

sum(vals_outer[[2]])/length(vals_outer[[2]])
sum(vals_inner[[2]])/length(vals_inner[[2]])


extract_annulus_profile(dcoord, woody_binary, annulus_radii,radius_step)



woody_binary <- as.numeric(woody_map)
buffer_outer <- st_buffer(dcoord, dist = 50)
vals_outer <- extract(woody_binary, buffer_outer)

extract_annulus_profile <- function(point_coords, raster, radii, step) {

    results <- data.frame(radius = numeric(), prop_noncrop = numeric())
  
  for (r in radii) {
    r_inner <- r - step
    r_outer <- r
    
    buffer_outer <- st_buffer(point_coords, dist = r_outer)
    buffer_inner <- st_buffer(point_coords, dist = r_inner)
    
    vals_outer <- extract(raster, buffer_outer)
    vals_inner <- extract(raster, buffer_inner)

    if (length(vals_outer[[2]]) > 0 & length(vals_inner[[2]]) > 0) {
      pixels_annulus <- vals_outer[[2]][!(vals_outer[[2]] %in% vals_inner[[2]])]
      if (length(pixels_annulus) > 0) {
        prop <- mean(pixels_annulus, na.rm = TRUE)
      } else {
        prop <- NA
      }
    } else {
      prop <- NA
    }
    
    results <- rbind(results, data.frame(radius = r, prop_noncrop = prop))
  }
  
  return(results)
}



# 2.3 Apply to all fields (with parallelization for speed)
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)

# Export functions and objects to clusters
clusterExport(cl, c("woody_map", "annulus_radii", "radius_step", 
                    "extract_annulus_profile"), 
              envir = environment())
clusterEvalQ(cl, {
  library(terra)
  library(sf)
})

# Extract profiles for each field
field_profiles <- parLapply(cl, 
                            seq_len(nrow(fields_sf)),
                            function(i) {
                              profile <- extract_annulus_profile(
                                st_geometry(fields_sf[i, ]),
                                woody_map,
                                annulus_radii,
                                radius_step
                              )
                              profile$Name <- fields_sf$Name[i]
                              profile$municipal_district <- fields_sf$municipal_district[i]
                              return(profile)
                            }
)

stopCluster(cl)

# Combine results
profiles_df <- do.call(rbind, field_profiles)
rownames(profiles_df) <- NULL

print("Annulus profiles extracted")
print(head(profiles_df))

# 2.4 Data quality checks
# Remove fields with too many NAs
min_valid_radii <- 0.7 * length(annulus_radii)
valid_fields <- profiles_df %>%
  group_by(Name) %>%
  summarise(n_valid = sum(!is.na(prop_noncrop))) %>%
  filter(n_valid >= min_valid_radii) %>%
  pull(Name)

profiles_df <- profiles_df %>% filter(Name %in% valid_fields)
fields_sf <- fields_sf %>% filter(Name %in% valid_fields)

print(paste("Fields retained after QC:", n_distinct(profiles_df$Name)))

# ============================================================================
# PART 3: PREPARE DATA FOR FUNCTIONAL REGRESSION
# ============================================================================

# 3.1 Create wide format for FDA (each row = field, columns = radii)
profiles_wide <- profiles_df %>%
  pivot_wider(
    names_from = radius,
    values_from = prop_noncrop,
    names_prefix = "radius_"
  )

# Create matrix format for refund package
Names <- profiles_wide$Name
Y_matrix <- as.matrix(profiles_wide[, -c(1, 2)])  # Remove Name and municipal_district
municipal_districts_vec <- profiles_wide$municipal_district
Y_matrix <- apply(Y_matrix, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# 3.2 Add field covariates
field_data <- st_drop_geometry(fields_sf) %>%
  select(Name, x, y, field_area) %>%
  filter(Name %in% Names)

# Standardize covariates for model stability
field_data <- field_data %>%
  mutate(
    x_std = scale(x)[, 1],
    y_std = scale(y)[, 1],
    area_log_std = scale(log(field_area))[, 1]
  )

# ============================================================================
# PART 4: FIT FUNCTION-ON-SCALAR REGRESSION MODELS
# ============================================================================

# 4.1 Prepare functional data object
argvals <- annulus_radii  # Distance values

# 4.2 Fit model for each municipal district
# Model equation: Y(r) = α + β(md) + μ(md, r) + ε(r)
# where α = overall mean, β(md) = district constant, μ(md, r) = district function
# Includes smooth spatial trend and field area control

# Create model formula for refund::pffr
fit_district_models <- function(district_name, Y, argvals, field_data, municipal_districts_vec) {
  """
  Fit function-on-scalar regression for a single municipal district
  """
  
  # Subset data for this district
  idx <- municipal_districts_vec == district_name
  Y_dist <- Y[idx, ]
  fd_obj <- t(Y_dist)  # Transpose for refund format (radii × fields)
  
  # Corresponding field data
  field_data_dist <- field_data[idx, ]
  
  # Fit model using pffr (function-on-scalar regression)
  # pffr(Y ~ x + y + s(area))
  tryCatch({
    model <- pffr(
      fd_obj ~ 1 + 
        te(x_std, y_std, bs = "tp", m = c(2, 2)) +  # 2D tensor product smooth for spatial variation
        s(area_log_std, bs = "tp", m = 2),           # Spline for field area effect
      yind = argvals,
      data = field_data_dist,
      family = "binomial"  # Appropriate for proportion data
    )
    return(list(model = model, district = district_name, n_fields = nrow(Y_dist)))
  }, error = function(e) {
    warning(paste("Model fitting failed for district", district_name, ":", e$message))
    return(NULL)
  })
}

# Get unique districts
districts <- unique(municipal_districts_vec)

# Fit models for each district
models_list <- lapply(districts, function(d) {
  cat(paste("Fitting model for district:", d, "\n"))
  fit_district_models(d, Y_matrix, argvals, field_data, municipal_districts_vec)
})

names(models_list) <- districts
models_list <- models_list[!sapply(models_list, is.null)]

print(paste("Successfully fitted", length(models_list), "district models"))

# ============================================================================
# PART 5: EXTRACT PREDICTED LANDSCAPE COMPLEXITY FUNCTIONS
# ============================================================================

# 5.1 Predict mean function for each district at average covariate values
predicted_functions <- list()

for (district in names(models_list)) {
  model <- models_list[[district]]$model
  
  # Get average covariate values for the district
  district_avg <- field_data %>%
    filter(municipal_districts_vec == district) %>%
    summarise(
      x_std_avg = mean(x_std, na.rm = TRUE),
      y_std_avg = mean(y_std, na.rm = TRUE),
      area_log_std_avg = mean(area_log_std, na.rm = TRUE)
    )
  
  # Create prediction data
  newdata <- data.frame(
    x_std = district_avg$x_std_avg,
    y_std = district_avg$y_std_avg,
    area_log_std = district_avg$area_log_std_avg
  )
  
  # Predict
  tryCatch({
    pred <- predict(model, newdata = newdata, se.fit = TRUE)
    predicted_functions[[district]] <- data.frame(
      district = district,
      radius = argvals,
      mean_prop = plogis(pred$fit[1, ]),  # Convert from logit scale
      se = pred$se.fit[1, ]
    )
  }, error = function(e) {
    warning(paste("Prediction failed for district", district, ":", e$message))
  })
}

# Combine predictions
landscape_complexity_functions <- do.call(rbind, predicted_functions)
rownames(landscape_complexity_functions) <- NULL

print("Predicted landscape complexity functions obtained")
print(head(landscape_complexity_functions, 10))

# ============================================================================
# PART 6: VISUALIZATION AND DIAGNOSTICS
# ============================================================================

# 6.1 Plot predicted landscape complexity functions by district
p1 <- ggplot(landscape_complexity_functions, 
             aes(x = radius, y = mean_prop, colour = district, fill = district)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = mean_prop - 1.96*se, ymax = mean_prop + 1.96*se), 
              alpha = 0.2, color = NA) +
  facet_wrap(~district) +
  theme_minimal() +
  labs(
    title = "Predicted Landscape Complexity Functions by Municipal District",
    x = "Distance from field centre (m)",
    y = "Proportion of non-crop land cover",
    color = "District",
    fill = "District"
  ) +
  theme(legend.position = "bottom")

print(p1)
ggsave("landscape_complexity_functions.png", p1, width = 12, height = 8, dpi = 300)

# 6.2 Plot observed vs predicted functions (sample)
profiles_plot <- profiles_df %>%
  group_by(municipal_district, radius) %>%
  summarise(
    mean_obs = mean(prop_noncrop, na.rm = TRUE),
    se_obs = sd(prop_noncrop, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  rename(district = municipal_district)

p2 <- ggplot() +
  geom_line(data = profiles_plot, 
            aes(x = radius, y = mean_obs, color = district, linetype = "Observed"),
            linewidth = 1) +
  geom_line(data = landscape_complexity_functions,
            aes(x = radius, y = mean_prop, color = district, linetype = "Predicted"),
            linewidth = 1) +
  facet_wrap(~district) +
  theme_minimal() +
  labs(
    title = "Observed vs Predicted Landscape Complexity",
    x = "Distance from field centre (m)",
    y = "Proportion of non-crop land cover",
    color = "District",
    linetype = ""
  ) +
  theme(legend.position = "bottom")

print(p2)
ggsave("observed_vs_predicted_complexity.png", p2, width = 12, height = 8, dpi = 300)

# 6.3 Model diagnostics
for (district in names(models_list)) {
  cat("\n=== DIAGNOSTICS FOR DISTRICT:", district, "===\n")
  model <- models_list[[district]]$model
  
  cat("Model summary:\n")
  print(summary(model))
  
  # Deviance explained
  dev_expl <- (model$null.deviance - model$deviance) / model$null.deviance * 100
  cat("Deviance explained:", round(dev_expl, 2), "%\n")
}

# 6.4 Concurvity check (multicollinearity in smooths)
cat("\n=== CONCURVITY ANALYSIS ===\n")
for (district in names(models_list)) {
  model <- models_list[[district]]$model
  cat("District:", district, "\n")
  print(concurvity(model))
}

# ============================================================================
# PART 7: CREATE COMPLEXITY SCORES FOR FIELDS (OPTIONAL)
# ============================================================================

# 7.1 Calculate integrated landscape complexity metric for each field
field_complexity_scores <- profiles_df %>%
  group_by(Name, municipal_district) %>%
  summarise(
    # Area under the curve (AUC) - integrated complexity
    integrated_complexity = mean(prop_noncrop, na.rm = TRUE),
    
    # Proximity complexity (mean of inner radii)
    proximity_complexity = mean(prop_noncrop[radius <= 200], na.rm = TRUE),
    
    # Landscape complexity (mean of outer radii)
    landscape_complexity = mean(prop_noncrop[radius > 200], na.rm = TRUE),
    
    # Complexity gradient (rate of change)
    n_radii = sum(!is.na(prop_noncrop))
  ) %>%
  filter(n_radii >= min_valid_radii * 0.8) %>%
  select(-n_radii)

# Merge with original field data
fields_with_complexity <- fields_sf %>%
  left_join(st_drop_geometry(field_complexity_scores), 
            by = c("Name", "municipal_district"))

print("\nField complexity scores calculated")
print(head(fields_with_complexity, 10))

# ============================================================================
# PART 8: EXPORT RESULTS
# ============================================================================

# 8.1 Save predicted functions
write.csv(landscape_complexity_functions, 
          "landscape_complexity_functions.csv", 
          row.names = FALSE)

# 8.2 Save field complexity scores
st_write(fields_with_complexity, 
         "fields_with_complexity_scores.gpkg", 
         delete_dsn = TRUE)

# 8.3 Save models for later use (optional)
saveRDS(models_list, "landscape_complexity_models.RDS")

print("\nAnalysis complete! Results saved to working directory")
print("Files generated:")
print("  - landscape_complexity_functions.csv")
print("  - fields_with_complexity_scores.gpkg")
print("  - landscape_complexity_models.RDS")
print("  - landscape_complexity_functions.png")
print("  - observed_vs_predicted_complexity.png")

# ============================================================================
# SUGGESTED ENHANCEMENTS FOR STRONGER ANALYSIS
# ============================================================================

# 1. CROSS-VALIDATION
# Add k-fold cross-validation to assess model generalizability
# Particularly important for landscape ecology studies

# 2. ALTERNATIVE FUNCTIONAL BASES
# Consider B-spline basis functions instead of penalized splines
# Provides better control over smoothness and interpretability

# 3. SPATIAL AUTOCORRELATION
# Test for spatial autocorrelation in model residuals
# Consider adding CAR (conditional autoregressive) smooths

# 4. MULTIPLE WOODY FEATURE TYPES
# If possible, model different woody feature types separately
# (trees, shrubs, hedgerows) and create composite index

# 5. TEMPORAL DYNAMICS (if data available)
# Extend to panel data if woody features mapped over multiple years
# Use functional time series models

# 6. HIERARCHICAL STRUCTURE
# Incorporate nested structure (fields within districts within regions)
# Use multilevel FDA models

# 7. UNCERTAINTY QUANTIFICATION
# Bootstrap confidence intervals for predicted functions
# Propagate mapping uncertainty into analysis

# 8. SENSITIVITY ANALYSIS
# Test sensitivity to annulus width, max radius, and knot settings
# Create robustness check table

# 9. COMPARE WITH TRADITIONAL METRICS
# Calculate landscape metrics (diversity, fragmentation, LSI)
# Compare explanatory power vs FDA approach

# 10. ECOLOGICAL VALIDATION
# Validate predicted complexity against field observations
# Survey actual non-crop features to ground-truth predictions