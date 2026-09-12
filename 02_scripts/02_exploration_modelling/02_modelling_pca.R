fieldpalette = 
  c("#577590",
    "#4d908e",
    "#43aa8b",
    "#90be6d",
    "#f9c74f",
    "#f8961e",
    "#f3722c",
    "#f94144")


# Load Data ---------------------------------------------------------------
rm(list=ls())
df <- read.csv("01_Data/dffinal_20260909.csv")
df <- df |> mutate(yield_log = log(yield_tha+2), yield_scaled = as.numeric(scale(yield_log)))
table(df$field)
colnames(df)


# factors
year <- as.factor(df$year)
id <- as.factor(df$id)
field <- as.factor(df$field)
year_treecut <- as.factor(df$harvestyear)
crop <- as.factor(df$crop_unified)

# distance predictor 
tree_distance <- as.numeric(df$distance_to_tree_strip)

# tree characteristics
treeage <- as.numeric(df$treeage)
AFage <- as.numeric(df$AFage)

# climate 
lat <- as.numeric(df$latitude)
long <- as.numeric(df$longitude)
temp <- as.numeric(df$temp_C_mean)
sun <- as.numeric(df$sun_MJ_m2_mean)
precip <- as.numeric(df$precip_mm_sum)
min_slope <- as.numeric(df$min_slope)
mean_slope <- as.numeric(df$mean_slope)
max_slope <- as.numeric(df$max_slope)
clay <- as.numeric(df$clay)
sand <- as.numeric(df$sand)
silt <- as.numeric(df$silt)

# landscape
fieldlength <- as.numeric(df$fieldlength)
contag <- as.numeric(df$l_contag)
simpson <- as.numeric(df$l_sidi)
shannon <- as.numeric(df$l_shdi)
patchnumber <- as.numeric(df$l_np)
aggregation <- as.numeric(df$l_ai)
edgedensity <- as.numeric(df$l_ed)

# small woody features
swf_year <- as.factor(df$swf_year)
swf_distance <- as.numeric(df$distance)
swf_prop <- as.numeric(df$prop_swf)
swf_field <- as.numeric(df$prop_swf_within)



# original values #############################
o <- df |> distinct(
  field, year, fieldlength, l_contag, l_sidi,
  l_shdi,l_np , l_ai, l_ed, swf_year, prop_swf_within,
  latitude, longitude, temp_C_mean, sun_MJ_m2_mean,
  precip_mm_sum, min_slope, mean_slope, max_slope,
  clay, sand, silt, treeage, AFage
)
nrow(o) # 34


# yield values #############################

y <- df |> distinct(field, year, crop_unified, distance_to_tree_strip, yield_tha)

x <- df |> distinct(field, year, crop_unified, distance_to_tree_strip, yield_tha, ID_nodist)
xs <- df |> distinct(field, year, crop_unified, distance_to_tree_strip, yield_tha, ID_nodist, swf_year)

z <- df |> select(-c("radius","distance", "radius.x", "radius.y", "prop_swf", "swf_year" )) |> distinct()

nrow(xs) # 1171
nrow(y) # 1158
nrow(x) # 1171
nrow(z) # 1171

comparison <- x |>
  group_by(field, year, crop_unified, distance_to_tree_strip, yield_tha) |>
  summarise(
    n_ID_nodist = n_distinct(ID_nodist),
    ID_nodist = paste(unique(ID_nodist), collapse = ", "),
    .groups = "drop"
  ) |>
  filter(n_ID_nodist > 1)

comparison # all good, all okay


# relationship in yield: 
# for each distance to tree strip there is a value of yield 

plot(x$yield_tha ~ x$distance_to_tree_strip)
# it is a functional relationship 

# continous predictor variable #############################

s <- df |> distinct(field, swf_year, prop_swf, distance, ID_nodist)

nrow(s) # 180

plot(s$prop_swf ~ s$distance)
# also a functional relationship





# MODELS new --------------------------------------------------------------
library(mgcv)
library(lme4)
library(nlme)
library(ggplot2)

m1 <- lme(fixed = yield ~ tree_dist + crop_unified,
  random = ~ 1 | field/year,
  data = z,
  na.action = na.omit,
  method = "REML"
)

summary(m1)
fixef(m1)
intervals(m1)


# Extract fitted values and residuals
diagnostics <- data.frame(
  fitted = fitted(m1),
  residuals = resid(m1, type = "normalized"),
  standardized = resid(m1, type = "pearson")
)

# 1. Residuals vs fitted values
ggplot(diagnostics, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Fitted values",
    y = "Normalized residuals",
    title = "Residuals vs fitted values"
  ) +
  theme_classic()


# 2. Normal Q-Q plot of residuals
ggplot(diagnostics, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Normal Q-Q plot of residuals"
  ) +
  theme_classic()


# 3. Histogram of residuals
ggplot(diagnostics, aes(x = residuals)) +
  geom_histogram(bins = 30) +
  labs(
    x = "Normalized residuals",
    y = "Count",
    title = "Distribution of residuals"
  ) +
  theme_classic()


# 4. Residuals against tree distance
ggplot(
  cbind(z, diagnostics),
  aes(x = tree_dist, y = residuals)
) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Tree distance",
    y = "Normalized residuals",
    title = "Residuals vs tree distance"
  ) +
  theme_classic()

# Random effects
ranef(m1)

# Q-Q plot of random effects
qqnorm(m1, ~ ranef(.))
qqline(m1, ~ ranef(.))


ggplot(z, aes(x = tree_dist, y = yield)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    x = "Tree distance",
    y = "Yield",
    title = "Yield vs tree distance"
  ) +
  theme_classic()

z <- z |> 
  mutate(yield = yield_log,
         tree_dist = log(distance_to_tree_strip))

m1 <- lme(z, yield ~ tree_dist + crop_unified + 1/field + 1/year)

m1 <- gam(z, yield ~ )



# MODELS old------------------------------------------------------------------

df <- df |>
  mutate(
    field = factor(field),
    year  = factor(year),
    crop_unified = factor(crop_unified)
  )
x <- x |>
  mutate(
    field = factor(field),
    year  = factor(year),
    crop_unified = factor(crop_unified)
  )
o <- o |>
  mutate(
    field = factor(field),
    year  = factor(year)
  )
s <- s |>
  mutate(
    field = factor(field),
    swf_year  = factor(swf_year)
  )



# Core model
m1 <- bam(
  yield_tha ~
    s(distance_to_tree_strip, k = 7) +   # nonlinear distance effect
    crop_unified +                          # crop as fixed factor
    s(field, bs = "re") +                  # random intercept: field
    s(year,  bs = "re"),                   # random intercept: year
  data = x,
  method = "REML"
)

summary(m1)
gratia::draw(m1)

# Option A: add a handful of theoretically motivated predictors
m2 <- bam(
  yield_tha ~
    s(distance_to_tree_strip, k = 7) +
    crop_unified +
    treeage + temp_C_mean + clay +        # parsimonious field-level set
    s(field, bs = "re") +
    s(year,  bs = "re"),
  data = x |> left_join(o, by = c("field", "year")),
  method = "fREML"
)

summary(m2)
gratia::draw(m2)


# Option B: PCA the correlated landscape metrics first, use PCs
library(FactoMineR)
landscape_pca <- PCA(o |> select(l_contag:l_ed), scale.unit = TRUE)
# 

x_full <- xs |>
  left_join(s, by = c("ID_nodist", "swf_year"))  # adjust keys

m3 <- bam(
  yield_tha ~
    s(distance_to_tree_strip, k = 7) +
    s(prop_swf, k = 6) +                  # or ti() for interaction
    crop_unified +
    s(field, bs = "re") + s(year, bs = "re"),
  data = x_full,
  method = "fREML"
)
summary(m3)
gratia::draw(m3)


library(gratia)
appraise(m1)      # residual diagnostics
draw(m1)          # visualize smooth terms
concurvity(m1)    # check for concurvity (like collinearity for smooths)


AIC(m1, m2, m3)




# MODELS 12/9/2026 --------------------------------------------------------

library(mgcv)
library(itsadug)

# 
# The basic workflow of GAMM is as follows:
#   
# (1) Create an ordered factor for the factors of interest
# (2) Fit the model with gam() or bam()
# (3) Handling autocorrelation errors
# (4) Model criticism
# (5) Visualization of results
# (6) Significance testing


library(FactoMineR)
landscape_pca <- PCA(o |> select(l_contag:l_ed), scale.unit = TRUE)



# PCA  --------------------------------------------------------------------


library(ggcorrplot)
library('corrr')
library("FactoMineR")
library(factoextra)

data_normalized <- scale(numerical_data)
head(data_normalized)


# factors
year <- as.factor(z$year)
id <- as.factor(z$id)
field <- as.factor(z$field)
year_treecut <- as.factor(z$harvestyear)
crop <- as.factor(z$crop_unified)

# distance predictor 
tree_distance <- as.numeric(z$distance_to_tree_strip)

# tree characteristics
treeage <- as.numeric(z$treeage)
AFage <- as.numeric(z$AFage)

tree <- data.frame(treeage,AFage,swf_field)
tree_s <- scale(tree)

corr_matrix <- cor(tree_s)
ggcorrplot(corr_matrix,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

# climate # climate # climate 
lat <- as.numeric(z$latitude)
long <- as.numeric(z$longitude)
temp <- as.numeric(z$temp_C_mean)
sun <- as.numeric(z$sun_MJ_m2_mean)
precip <- as.numeric(z$precip_mm_sum)
min_slope <- as.numeric(z$min_slope)
mean_slope <- as.numeric(z$mean_slope)
max_slope <- as.numeric(z$max_slope)
clay <- as.numeric(z$clay)
sand <- as.numeric(z$sand)
silt <- as.numeric(z$silt)

climate <- data.frame(temp, sun, precip)
climate_s <- scale(climate)

corr_matrix <- cor(climate_s)
ggcorrplot(corr_matrix,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

data.pca_c <- princomp(climate_s)
(pca_sum_c <- summary(data.pca_c))

soil <- data.frame(clay, sand, silt)
soil_s <- scale(soil)

data.pca_s <- princomp(soil_s)
(pca_sum_s <- summary(data.pca_s))


corr_matrix <- cor(soil_s)
ggcorrplot(corr_matrix,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

# landscape
fieldlength <- as.numeric(z$fieldlength)
contag <- as.numeric(z$l_contag)
simpson <- as.numeric(z$l_sidi)
shannon <- as.numeric(z$l_shdi)
patchnumber <- as.numeric(z$l_np)
aggregation <- as.numeric(z$l_ai)
edgedensity <- as.numeric(z$l_ed)

landscape <- data.frame(field, contag, simpson, shannon, patchnumber, aggregation, edgedensity)
landscape_s <- scale(landscape[-1])


data.pca_l <- princomp(landscape_s)
(pca_sum_l <- summary(data.pca_l))

fviz_pca_ind(
  data.pca_l,
  col.ind = landscape$field,
  palette = palette,
  #pointshape = 16,     # filled circle for all observations
  #pointsize = 3.5,     # larger points
  title = "Landscape PCA including all landscape indices"
)
corr_matrix <- cor(landscape_s)
ggcorrplot(corr_matrix,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

# small woody features
swf_year <- as.factor(z$swf_year)
swf_distance <- as.numeric(z$distance)
swf_prop <- as.numeric(z$prop_swf)
swf_field <- as.numeric(z$prop_swf_within)
