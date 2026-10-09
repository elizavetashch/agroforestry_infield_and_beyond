# models after conference 

#dflong <- read.csv("01_Data/20260920_moddata.csv")

library(dplyr)
library(tidyverse)
library(mgcv) 
library(ggplot2)
library(tidyr)

swf_mat <- read.csv("01_Data/20260920_swf_mat.csv")
df <- read_csv("01_Data/AF_swf.csv")  

dflong <- df  |> 
  group_by(field, year, crop_unified, distance_to_tree_strip) |>
  summarise(
    yield_tha     = mean(yield_tha, na.rm = TRUE),
    fert_N        = mean(fert_N), # fertilidflongation
    temp_C_mean   = first(temp_C_mean), # climate
    precip_mm_sum = first(precip_mm_sum), # climate
    sun_MJ_m2_mean= first(sun_MJ_m2_mean), # climate
    AFage         = first(AFage), # AFdesign
    treeage         = first(treeage), # AFdesign
    clay          = first(clay), #soil
    sand          = first(sand), #soil
    silt          = first(silt), #soil
    l_shdi        = first(l_shdi), # landscape
    l_ed          = first(l_ed), # landscpae
    l_np          = first(l_np), # landscape
    l_contag      = first(l_contag), # landscape
    mean_slope    = first(mean_slope), # slope
    prop_swf_within = first(prop_swf_within),
    sowing_month = first(as.factor(sowing_month)),
    harvest_month = first(as.factor(harvest_month)),
    .groups = "drop"
  )  |> 
  mutate(
    photo_path   = factor(ifelse(crop_unified == "maidflonge", "C4", "C3")),
    log_AFage    = log1p(AFage),
    log_dist     = log(distance_to_tree_strip),
    crop_unified = factor(crop_unified),
    field        = factor(field),
    year         = factor(year),
    dist_bin     = factor(distance_to_tree_strip),
    # yield relative to unit mean (for interpretation)
    .by = c(field, year, crop_unified)
  ) |>
  group_by(field, year, crop_unified) |>
  mutate(yield_rel = yield_tha / mean(yield_tha)) |>
  ungroup()


dflong$crop_season <- ifelse(
  dflong$sowing_month %in% c("Oct", "Nov", "Jan", "Sep"),
  "winter",
  ifelse(
    dflong$sowing_month %in% c("Mar", "Apr", "May"),
    "summer",
    NA
  )
)
dflong$crop_season <- as.factor(dflong$crop_season)
table(as.factor(dflong$crop_season))
table(as.factor(dflong$crop_season), as.factor(dflong$crop_unified))
# summer winter 
# 43     97




# HYPOTHESIS 1:  ----------------------------------------------------------

# Yield is influenced by distance to the tree strip. The larger the distance
# the larger the yield. Yet at medium distances yield can get saturated and 
# might decline to larger distances. 
levels(as.factor(dflong$distance_to_tree_strip))
m_00 <- gam(yield_rel ~ s(distance_to_tree_strip, k = 5) + 
            s(year, bs = "re"),
            data = dflong,
            family = gaussian,
            method = "REML"
)

summary(m_00)
k.check(m_00)
par(mfrow = c(2, 2))
gam.check(m_00, pch = 16, cex = 0.5)
par(mfrow = c(1, 1))
plot(m_00, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE, scale = 0, residuals = TRUE,
     pch = 16, cex = 0.3, col = "grey50")

AIC(m_00) 

m_01 <- gam(yield_rel ~ 
              s(distance_to_tree_strip, k = 5) + 
              crop_season + 
            s(year, bs = "re"),
            data = dflong,
            family = gaussian,
            method = "REML"
)

summary(m_01)
k.check(m_01)
par(mfrow = c(2, 2))
gam.check(m_01, pch = 16, cex = 0.5)
par(mfrow = c(1, 1))
plot(m_01, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE, scale = 0, residuals = TRUE,
     pch = 16, cex = 0.3, col = "grey50")

AIC(m_01)


m_02 <- gam(yield_rel ~ s(distance_to_tree_strip, k = 5) + 
              s(crop_season, bs = "re") + 
              s(year, bs = "re"),
            data = dflong,
            family = gaussian,
            method = "REML"
)
summary(m_02)
k.check(m_02)
par(mfrow = c(2, 2))
gam.check(m_02, pch = 16, cex = 0.5)
par(mfrow = c(1, 1))
plot(m_02, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE, scale = 0, residuals = TRUE,
     pch = 16, cex = 0.3, col = "grey50")
AIC(m_02) # slightly better than m_00

m_03 <- gam(yield_rel ~ s(distance_to_tree_strip, k = 5) + 
              ti(distance_to_tree_strip, treeage, k = c(5, 5)) +
              s(year, bs = "re"),
            data = dflong,
            family = gaussian,
            method = "REML"
)
# HYP 1 VISUALISATION -----------------------------------------------------


df_means <- dflong %>%
  group_by(field, year, crop_season, distance_to_tree_strip) %>%
  summarise(yield_rel = mean(yield_rel, na.rm = TRUE), .groups = "drop")

ggplot(dflong, aes(distance_to_tree_strip, yield_rel)) +
  geom_jitter(aes(colour = crop_season),
              width = 0.3, height = 0, alpha = 0.25, size = 1.2) +
  geom_line(data = df_means,
            aes(group = interaction(field, year), colour = crop_season),
            alpha = 0.5, linewidth = 0.4) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5),
              colour = "black", fill = "grey60", linewidth = 1.1) +
  stat_summary(fun.data = mean_se, geom = "pointrange",
               colour = "black", size = 0.4) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
  labs(x = "Distance to tree strip (m)",
       y = "Relative yield",
       colour = "Crop / season") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

# the variation in summer crops is much larger than in winter crops 

dflong %>%
  filter(crop_season == "summer", distance_to_tree_strip == 1) %>%   # adjust
  summarise(
    n      = n(),
    mean   = mean(yield_rel, na.rm = TRUE),
    sd     = sd(yield_rel, na.rm = TRUE),
    cv_pct = 100 * sd / mean,                 # comparable across distances/crops
    median = median(yield_rel, na.rm = TRUE),
    iqr    = IQR(yield_rel, na.rm = TRUE),
    min    = min(yield_rel, na.rm = TRUE),
    max    = max(yield_rel, na.rm = TRUE)
  )

dflong %>%
  filter(crop_season == "winter", distance_to_tree_strip == 1) %>%   # adjust
  summarise(
    n      = n(),
    mean   = mean(yield_rel, na.rm = TRUE),
    sd     = sd(yield_rel, na.rm = TRUE),
    cv_pct = 100 * sd / mean,                 
    median = median(yield_rel, na.rm = TRUE),
    iqr    = IQR(yield_rel, na.rm = TRUE),
    min    = min(yield_rel, na.rm = TRUE),
    max    = max(yield_rel, na.rm = TRUE)
  )

dflong %>%
  filter(crop_season == "summer", distance_to_tree_strip == 24) %>%   # adjust
  summarise(
    n      = n(),
    mean   = mean(yield_rel, na.rm = TRUE),
    sd     = sd(yield_rel, na.rm = TRUE),
    cv_pct = 100 * sd / mean,                 
    median = median(yield_rel, na.rm = TRUE),
    iqr    = IQR(yield_rel, na.rm = TRUE),
    min    = min(yield_rel, na.rm = TRUE),
    max    = max(yield_rel, na.rm = TRUE)
  )

dflong %>%
  filter(crop_season == "winter", distance_to_tree_strip == 24) %>%   
  summarise(
    n      = n(),
    mean   = mean(yield_rel, na.rm = TRUE),
    sd     = sd(yield_rel, na.rm = TRUE),
    cv_pct = 100 * sd / mean,                
    median = median(yield_rel, na.rm = TRUE),
    iqr    = IQR(yield_rel, na.rm = TRUE),
    min    = min(yield_rel, na.rm = TRUE),
    max    = max(yield_rel, na.rm = TRUE)
  )


# if the variation within summer crops is so much larger, can we argue
# that those are exactly the spill over effects that are prominent in the summer? 
# it is just a wild hypothesis. can i test for it? 


# HYPOTHESIS 2 ------------------------------------------------------------

# small woody features influence the yield 


radii <- seq(100, 1000, by = 100)

# swf_mat must have exactly one row per row of dflong, in the same order
stopifnot(nrow(swf_mat) == nrow(dflong))

# put both matrices INTO the data frame
dflong$R   <- matrix(radii, nrow = nrow(dflong), ncol = length(radii), byrow = TRUE)
dflong$SWF <- as.matrix(swf_mat)

# random-effect terms need factors
dflong$crop_season <- factor(dflong$crop_season)
dflong$year        <- factor(dflong$year)
dflong$field       <- factor(dflong$field)

# ID for each distinct SWF curve (see note below)
dflong$swf_id <- factor(apply(round(swf_mat, 10), 1, paste, collapse = "_"))

m_2tha <- gam(yield_tha ~ s(R, by = SWF, k = 4) + 
             s(crop_season,  bs = "re") +
             s(year, bs = "re"),
           data = dflong, method = "REML")

summary(m_2tha)
plot(m_2tha, select = 1, shade = TRUE, seWithMean = TRUE,
     xlab = "Radius (m)", ylab = expression(beta(r)))
abline(h = 0, lty = 2)

m_2 <- gam(yield_rel ~ 
             s(R, by = SWF, k = 4) + 
             s(crop_season, bs = "re") +
             s(year, bs = "re"),
           data = dflong, method = "REML")

summary(m_2)
plot(m_2, select = 1, shade = TRUE, seWithMean = TRUE,
     xlab = "Radius (m)", ylab = expression(beta(r)))
abline(h = 0, lty = 2)

# alone small woody features do not explain any variation within the data. 
# however there is large variation in its effect closest and farest away from the field. 


# HYPOTHESIS 3 ------------------------------------------------------------
# variation in small woody features will improve the model with intrinsic 
# variiation 

m_3 <- gam(yield_rel ~ 
             s(distance_to_tree_strip, k = 5)+ 
             s(R, by = SWF, k = 4) + 
             s(crop_season, bs = "re") +
             s(year, bs = "re"),
           data = dflong, method = "REML")

gam.check(m_3)

# HYPOTHESIS 4  -----------------------------------------------------------
# spillover effects will arise from landscape composition and small woody features

m_4 <- gam(yield_rel ~ 
             s(distance_to_tree_strip, k = 5)+
             s(R, by = SWF, k = 4) + 
             l_shdi + l_ed + l_contag +  
             s(crop_season, bs = "re") +
             s(year, bs = "re"),
           data = dflong, method = "REML")
AIC(m_4)



# principal components ----------------------------------------------------
field        <- as.factor(dflong$field)
year         <- as.factor(dflong$year)

temp       <- as.numeric(dflong$temp_C_mean)
sun        <- as.numeric(dflong$sun_MJ_m2_mean)
precip     <- as.numeric(dflong$precip_mm_sum)
clay       <- as.numeric(dflong$clay)
sand       <- as.numeric(dflong$sand)
silt       <- as.numeric(dflong$silt)

climate   <- data.frame(field, year, temp, sun, precip)
climate_s <- scale(climate[-c(1:2)])
data.pca_c  <- princomp(climate_s)
soil   <- data.frame(field, year, clay, sand, silt)
soil_s <- scale(soil[-c(1:2)])
data.pca_s <- princomp(soil_s)
soil_pca      <- data.frame(year = soil$year,  field = soil$field,      PC1_s = data.pca_s$scores[, 1]) |> unique()
climate_pca   <- data.frame(year = climate$year,field = soil$field,     PC1_c = data.pca_c$scores[, 1])|> unique()

dflong <- dflong |>
  dplyr::left_join(soil_pca, by = c("field", "year")) |>
  dplyr::left_join(climate_pca, by = c("field", "year"))|> 
  mutate(field = as.factor(field),
         year = as.factor(year))

# GLOBAL MODEL ------------------------------------------------------------

m_global <- gam(yield_rel ~ 
                  s(distance_to_tree_strip, k = 5)+
                  s(R, by = SWF, k = 4) + 
                  l_shdi + l_ed + l_contag +  
                  treeage + 
                  AFage +
                  PC1_c +
                  PC1_s +
                  s(crop_season, bs = "re") +
                  s(year, bs = "re"),
                data = dflong, method = "REML")

summary(m_global)
AIC(m_global)
gam.check(m_global)
k.check(m_global)
par(mfrow = c(2, 2))
gam.check(m_global, pch = 16, cex = 0.5)




# GLOBAL subunit landscape ------------------------------------------------

m_gl_land <- gam(yield_rel ~ 
                  s(distance_to_tree_strip, k = 5)+
                  s(R, by = SWF, k = 4) + 
                  l_shdi + l_ed + l_contag +  
                  #treeage + 
                  #AFage +
                  #PC1_c +
                  #PC1_s +
                  s(crop_season, bs = "re") +
                  s(year, bs = "re"),
                data = dflong, method = "REML")
summary(m_gl_land)

m_gl_climate <- gam(yield_rel ~ 
                  s(distance_to_tree_strip, k = 5)+
                  s(R, by = SWF, k = 4) + 
                  #l_shdi + l_ed + l_contag +  
                  #treeage + 
                  #AFage +
                  PC1_c +
                  PC1_s +
                  s(crop_season, bs = "re") +
                  s(year, bs = "re"),
                data = dflong, method = "REML")
summary(m_gl_climate)

m_gl_system <- gam(yield_rel ~ 
                  s(distance_to_tree_strip, k = 5)+
                  #s(R, by = SWF, k = 4) + 
                  #l_shdi + l_ed + l_contag +  
                  treeage + 
                  AFage +
                  PC1_c +
                  PC1_s +
                  s(crop_season, bs = "re") +
                  s(year, bs = "re"),
                data = dflong, method = "REML")
summary(m_gl_system)



# write.csv(dflong, file = "01_Data/20261009_moddata")
