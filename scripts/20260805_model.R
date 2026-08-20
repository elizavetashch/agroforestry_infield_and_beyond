
rm(list=ls())
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(car)
library(lme4)
library(refund)

df <- read.csv("./analysis_data/20260805_AFslope.csv")

df <- df |> 
  mutate(year_planting= case_when(
    field == "Dornburg" ~ 2007,
    field == "Forst" ~ 2010,
    field == "Gladbacherhof" ~ 2020,
    field == "IhingerHof" ~ 2008,
    field == "Mariensee" ~ 2008,
    field == "Reiffenhausen" ~ 2011,
    field == "Vechta" ~ 2019,
    field == "Wendhausen" ~ 2008
  ))

write.csv(df, "./analysis_data/20260805_AFyearplanting.csv", row.names = FALSE)


# Collinearity check ------------------------------------------------------

dfnumeric <- df %>% select(where(is.numeric))
correlationmatrix <- cor(dfnumeric, use = "pairwise.complete.obs")
which(abs(correlationmatrix) > 0.7 & abs(correlationmatrix) < 1, arr.ind = TRUE)

library(corrplot)
corrplot(correlationmatrix, method = "color", type = "upper")


m0 <- lm(y ~ x1 + x2 + x3 + x4, data = dat)
mod <- lmer(yield_tha ~ mean_slope + temp_C_mean*precip_mm_sum*sun_MJ_m2_mean+
              l_contag + l_np + l_sidi + l_shdi + l_ai+l_ed+prop_swf_total+
              distance_to_tree_strip*treeage*year_planting + crop_unified +
              (1| data_id) + (1| field) + (1 | year), data = dfmod)

vif(mod)

dfmod <- df |> select(
  field, swf_year, year, data_id, crop_unified, yield_tha, mean_slope, temp_C_mean, precip_mm_sum, sun_MJ_m2_mean,
  distance_to_tree_strip, treeage, year_planting,
  l_contag, l_np, l_sidi, l_shdi, l_ai, l_ed
)

dfcategorical <- dfmod[,1:5]

swftotal <- df |> 
  group_by(swf_year, field) |> 
  mutate(prop_swf_total = sum(prop_swf)/((round(fieldlength / 2 / 100) * 100)+1000)) |> 
  select(swf_year, field, prop_swf_total) |> 
  unique()

dfmod <- left_join(dfmod, swftotal, by=c("field", "swf_year"))
dfnumeric <- dfmod[, 6:20]
dfnumeric <- scale(dfnumeric)
dfmod <- bind_cols(dfcategorical, dfnumeric)
colnames(dfmod)


#pairs(dfnumeric)
#vif(dfnumeric)


# correlation overview
scaled_mat <- apply(dfnumeric, 2, function(x) { 2 * (x - min(x)) / (max(x) - min(x)) - 1 })
corrplot::corrplot(scaled_mat)


mod <- lmer(yield_tha ~ mean_slope + temp_C_mean*precip_mm_sum*sun_MJ_m2_mean+
    l_contag + l_np + l_sidi + l_shdi + l_ai+l_ed+prop_swf_total+
    distance_to_tree_strip*treeage*year_planting + crop_unified +
    (1| data_id) + (1| field) + (1 | year), data = dfmod)

sjPlot::plot_model(mod)
sjPlot:: tab_model(mod) # saved in results from 20260805_allin_model
summary(mod)
# forgot fertilization rate! model it for sure.


set.seed(2121)

# Define the radial domain (0 to 10 mm from center)
radius <- seq(0, 10, length.out = n_radii)

# Create response: varies over radius
# (e.g., white matter density, diffusivity, etc.)
Y_radial <- matrix(NA, nrow = n_subjects, ncol = n_radii)
for(i in 1:n_subjects){
  # True underlying pattern: peak effect at r=3-4mm
  Y_radial[i, ] <- 100 - 10*radius + 5*sin(radius) + rnorm(n_radii, 0, 2)
}

# Create predictor: also varies over radius
# (e.g., marker concentration, temperature, etc.)
X1_radial <- matrix(NA, nrow = n_subjects, ncol = n_radii)
for(i in 1:n_subjects){
  X1_radial[i, ] <- 50 - 3*radius + rnorm(n_radii, 0, 1)
}

# Subject-level covariates (don't vary over radius)
subject_info <- data.frame(
  subject_id = 1:n_subjects,
  age = rnorm(n_subjects, mean=60, sd=15),
  group = rep(c("control", "lesion"), length.out=n_subjects)
)

# Combine into data frame
spatial_data <- cbind(subject_info,
                      Y = I(Y_radial),
                      X1 = I(X1_radial))

# Fit the model
model1 <- pffr(
  Y ~ ff(X1, xind = radius),  # Functional effect of X1 across radius
  yind = radius,               # Y measured at these radii
  data = spatial_data,
  family = gaussian()
)


colnames(df)
df <- df |> mutate(
  field_id = case_when(
    field == "Wendhausen" ~ 1,
    field == "Vechta" ~ 2,
    field == "Dornburg" ~ 3,
    field == "Reiffenhausen" ~ 4,
    field == "IhingerHof" ~ 5,
    field == "Gladbacherhof" ~ 6,
    field == "Mariensee" ~ 7,
    field == "Forst" ~ 8
    
  ))


model <- pffr(
  prop_swf ~ ff(1, xind=radius, bs="ps", k=42) + 
    ti(latitude, longitude, bs = c("tp", "tp"), k = c(4, 4)) + 
    ti(log(area), field_id, bs = c("tp", "tp"), k = c(4, 4)) +
    factor(field_id),
  yind = radius,
  data = df,
  family = binomial()
)

dfmod <- df[order(df$field_id, df$radius), ]
wide_resp <- reshape( df[, c("field_id", "radius", "prop_swf")], 
                      idvar = "field_id", 
                      timevar = "radius", 
                      direction = "wide" )
Y <- as.matrix(wide_resp[, -1])
radius <- as.numeric(sub("prop_swf\\.", "", colnames(Y)))
ord <- order(radius) 
radius <- radius[ord] 
Y <- Y[, ord]
wide_cov <- df[!duplicated(df$field_id),
               c("field_id", "latitude", "longitude", "area")]
  
wide_cov <- wide_cov[match(wide_resp$field_id, wide_cov$field_id), ]

dim(Y) # n_fields x n_radii 

length(radius) # should equal 
ncol(Y) nrow(wide_cov) # should equal 
nrow(Y) all(!is.na(radius))


model <- pffr(
  Y ~
    ff(1, xind = radius, bs = "ps", k = 72) +
    ti(latitude, longitude, bs = c("tp", "tp"), k = c(10, 10)) +
    s(field_id, bs = "re"),
  yind = radius,
  data = wide_cov,
  family = binomial(),
  sandwich = "none"
)

wide_cov <- df[!duplicated(df$field_id), c("field_id", "latitude", "longitude", "area")] 
wide_cov <- wide_cov[match(wide_resp$field_id, wide_cov$field_id), ]
model0 <- pffr( Y ~ ff(1, xind = radius, bs = "ps", k = 15), yind = radius, data = wide_cov )
