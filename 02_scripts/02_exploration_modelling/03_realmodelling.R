library(ggcorrplot)
library('corrr')
library("FactoMineR")
library(factoextra)
library(dplyr)
library(tidyverse)
library(mgcv)

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
df <- read.csv("01_Data/dffinal_20260909.csv")
df <- df |> mutate(yield_log = log(yield_tha+2), yield_scaled = as.numeric(scale(yield_log)),
                   distance_log = log(distance_to_tree_strip),
                   field = as.factor(field),
                   year = as.factor(year),
                   crop_unified = as.factor(crop_unified))
table(df$field)
colnames(df)

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

plot(x$yield_tha ~ x$distance_to_tree_strip)
# it is a functional relationship 

# continous predictor variable #############################

s <- df |> distinct(field, swf_year, prop_swf, distance, id)

nrow(s) # 180

plot(s$prop_swf ~ s$distance)
# also a functional relationship



# PCA ---------------------------------------------------------------------


# factors
year <- as.factor(z$year)
id <- as.factor(z$id)
field <- as.factor(z$field)
year_treecut <- as.factor(z$harvestyear)
crop <- as.factor(z$crop_unified)

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

climate <- data.frame(field, id, temp, sun, precip)
climate_s <- scale(climate[-c(1:2)])

corr_matrix_c <- cor(climate_s)
ggcorrplot(corr_matrix_c,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

data.pca_c <- princomp(climate_s)
(pca_sum_c <- summary(data.pca_c))

fviz_pca_ind(
  data.pca_c,
  label = FALSE, 
  col.ind = as.factor(climate$field),
  palette = fieldpalette,
  pointshape = 16,
  pointsize = 3.5,
  title = "Climate PCA including temperature, precipitation and solar radiation") +
  theme_classic() +
  theme(
    legend.title = element_blank()
  )

fviz_cos2(data.pca_c, choice = "var", axes = 1:2)

soil <- data.frame(field, id, clay, sand, silt)
soil_s <- scale(soil[-c(1:2)])

data.pca_s <- princomp(soil_s)
(pca_sum_s <- summary(data.pca_s))


corr_matrix_s <- cor(soil_s)
ggcorrplot(corr_matrix_s,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

fviz_pca_ind(
  data.pca_s,
  label = FALSE, 
  col.ind = as.factor(soil$field),
  palette = fieldpalette,
  pointshape = 16,
  pointsize = 3.5,
  title = "Soil PCA including soil texture") +
  theme_classic() +
  theme(
    legend.title = element_blank()
  )
fviz_cos2(data.pca_s, choice = "var", axes = 1:2)

# landscape
fieldlength <- as.numeric(z$fieldlength)
contag <- as.numeric(z$l_contag)
simpson <- as.numeric(z$l_sidi)
shannon <- as.numeric(z$l_shdi)
patchnumber <- as.numeric(z$l_np)
aggregation <- as.numeric(z$l_ai)
edgedensity <- as.numeric(z$l_ed)

landscape <- data.frame(field, id, contag, simpson, shannon, patchnumber, aggregation, edgedensity)
landscape_s <- scale(landscape[-c(1:2)])

corr_matrix_l <- cor(landscape_s)
ggcorrplot(corr_matrix_l,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

data.pca_l <- princomp(landscape_s)
(pca_sum_l <- summary(data.pca_l))

fviz_pca_ind(
  data.pca_l,
  label = FALSE, 
  col.ind = as.factor(landscape$field),
  palette = fieldpalette,
  pointshape = 16,
  pointsize = 3.5,
  title = "Landscape PCA including all landscape indices") +
  theme_classic() +
  theme(
    legend.title = element_blank()
  )

fviz_cos2(data.pca_l, choice = "var", axes = 1:2)





# PCA COMPONENTS ----------------------------------------------------------
pca_scores_l <- as.data.frame(data.pca_l$scores)
pca_scores_s <- as.data.frame(data.pca_s$scores)
pca_scores_c <- as.data.frame(data.pca_c$scores)

landscape_pca <- data.frame(id = landscape$id,PC1_l = data.pca_l$scores[, 1])
soil_pca <- data.frame(id = soil$id,PC1_s = data.pca_s$scores[, 1])
climate_pca <- data.frame(id = climate$id,PC1_c = data.pca_c$scores[, 1])

z <- z |> dplyr::left_join(landscape_pca, by = "id")
z <- z |> dplyr::left_join(soil_pca, by = "id")
z <- z |> dplyr::left_join(climate_pca, by = "id")
z <- z |> mutate(year_num = as.numeric(year))

# PCA TO MODELS -----------------------------------------------------------
plot(z$yield_log~z$distance_log )


# m0 ----------------------------------------------------------------------


m0 <- gam(
  yield_log ~ 
    PC1_c + PC1_l + PC1_s + 
    mean_slope + treeage + AFage + 
    s(crop_unified, bs = "re") + 
    te(lat, long, year_num),
  data = z,
  method = "REML"
)

summary(m0)


m0_dist <- gam(
  yield_log ~ 
    s(log(distance_to_tree_strip), k =3)+
    PC1_c + PC1_l + PC1_s + 
    mean_slope + treeage + AFage + 
    s(crop_unified,distance_to_tree_strip, bs = "re") + 
    te(lat, long, year_num),
  data = z,
  method = "REML"
)

summary(m0_dist)
AIC(m0_dist)





m0_pfr <- pfr(
  yield_log ~
    lf(swf_matrix, argvals = swf_argvals, k = 3) +
    s(log(distance_to_tree_strip), k =3)+
    PC1_c + PC1_l + PC1_s + 
    mean_slope + treeage + AFage + 
    s(crop_unified,distance_to_tree_strip, bs = "re") + 
    te(lat, long, year_num),
  data   = z_swf,
  method = "REML"
)

summary(m0_pfr)
AIC(m0_pfr)




m0_f <- gam(
  yield_log ~ 
    PC1_c + PC1_l + PC1_s + fert_N + 
    mean_slope + treeage + AFage + 
    s(crop_unified, bs = "re") + 
    te(lat, long, year_num),
  data = z,
  method = "REML"
)

summary(m0_f)
plot(m0_f, residuals = TRUE)
gam.check(m0_f)

AIC(m0_f)


# m1 ----------------------------------------------------------------------
m1 <- gam(
    yield_log ~ s(log(distance_to_tree_strip), k = 3) +
    s(crop_unified, bs = "re") + s(field, bs = "re") + s(year, bs = "re"),
    data = z,
    method = "REML"
)

summary(m1)
plot(m1, residuals = TRUE)
gam.check(m1)


m1_crop <- gam(
    yield_log ~ s(log(distance_to_tree_strip), k = 3) +
    s(crop_unified,distance_to_tree_strip, bs = "re") + s(field, bs = "re") + s(year, bs = "re"),
    data = z,
    method = "REML"
)

summary(m1_crop)
plot(m1_crop, residuals = TRUE)
gam.check(m1_crop)


# m2 ----------------------------------------------------------------------
m2_pcl <- gam(
    yield_log ~ s(log(distance_to_tree_strip), k = 3) + PC1_l +
      s(crop_unified,distance_to_tree_strip, bs = "re") + 
      s(field, bs = "re") + s(year, bs = "re"),
    data = z,
    method = "REML"
)
m2_pcs <- gam(
    yield_log ~ s(log(distance_to_tree_strip), k = 3) + PC1_s +
      s(crop_unified,distance_to_tree_strip, bs = "re") + 
      s(field, bs = "re") + s(year, bs = "re"),
    data = z,
    method = "REML"
)
m2_pcc <- gam(
    yield_log ~ s(log(distance_to_tree_strip), k = 3) + PC1_c +
      s(crop_unified,distance_to_tree_strip, bs = "re") + 
      s(field, bs = "re") + s(year, bs = "re"),
    data = z,
    method = "REML"
)

m2_pcsc <- gam(
    yield_log ~ s(log(distance_to_tree_strip), k = 3) + PC1_s + PC1_c +
      s(crop_unified,distance_to_tree_strip, bs = "re") + 
      s(field, bs = "re") + s(year, bs = "re"),
    data = z,
    method = "REML"
)

m2_pcscl <- gam(
    yield_log ~ s(log(distance_to_tree_strip), k = 3) + PC1_s  + PC1_l + PC1_c +
      s(crop_unified,distance_to_tree_strip, bs = "re") + 
      s(field, bs = "re") + s(year, bs = "re"),
    data = z,
    method = "REML"
)

summary(m2_pcl)
summary(m2_pcs)
summary(m2_pcc)

summary(m2_pcsc)
plot(m2_pcsc, residuals = TRUE)
gam.check(m2_pcsc)

summary(m2_pcscl)
plot(m2_pcscl, residuals = TRUE)
gam.check(m2_pcscl)

plot(m2_pcl, residuals = TRUE)
gam.check(m2_pcl)
gam.check(m2_pcs)
gam.check(m2_pcc)
gam.check(m2_pcsc)

AIC(m2_pcl, m2_pcs, m2_pcc, m2_pcsc)


# m3 ----------------------------------------------------------------------

m3_shannon <- gam(
  yield_log ~ s(log(distance_to_tree_strip), k = 3) + s(l_shdi) + PC1_s + PC1_c +
    s(crop_unified,distance_to_tree_strip, bs = "re") + 
    s(field, bs = "re") + s(year, bs = "re"),
  data = z,
  method = "REML"
)

summary(m3_shannon)
z$prop_swf_within
m3_swffield <- gam(
  yield_log ~ s(log(distance_to_tree_strip), k = 3) + s(prop_swf_within)+ s(l_shdi) + PC1_s + PC1_c +
    s(crop_unified,distance_to_tree_strip, bs = "re") + 
    s(field, bs = "re") + s(year, bs = "re"),
  data = z,
  method = "REML"
)

summary(m3_swffield)
gam.check(m3_swffield)
par(mfrow = c(2, 2))
plot(m3_swffield, residuals = TRUE)

AIC(m3_shannon)
gam.check(m3_shannon)
par(mfrow = c(2, 2))
plot(m3_shannon, residuals = TRUE)
family(m3_shannon)

dispersion <- sum(residuals(m3_shannon, type = "pearson")^2) / df.residual(m3_shannon)
dispersion
# if bigger than 1 then overdispersion 
# ~1 variance approximately as expected
# <1 underdispersion


# m4 ----------------------------------------------------------------------


m4 <- gam(
  yield_log ~ s(log(distance_to_tree_strip), k = 3) + s(prop_swf_within)+ PC1_l + PC1_s + PC1_c + s(fert_N) +
    s(crop_unified,distance_to_tree_strip, bs = "re") + 
    te(year_num, lat, long),
  data = z,
  method = "REML"
)

summary(m4)
gam.check(m4)
par(mfrow = c(1, 1))
plot(m4, residuals = TRUE)

m4_sl <- r2(m4)
m4_pcl <- r2(m4)


# refurnd model -----------------------------------------------------------


library(refund)
library(tidyverse)
str(s)
# parse year out of swf_year ("prop_swf.2015" -> 2015)
s_clean <- s |>
  mutate(year = as.integer(str_extract(swf_year, "\\d{4}"))) |>
  select(field, year, distance, prop_swf, id)

# one row per field x year, columns = distance points
swf_wide <- s_clean |>
  pivot_wider(
    names_from  = distance,
    values_from = prop_swf,
    names_prefix = "d_"
  ) |>
  arrange(id)

# the distance grid (argument values of the functional predictor)
swf_argvals <- sort(unique(s_clean$distance))  # e.g. 100, 200, ..., 1000

# join so each yield row gets the full SWF curve for its field x year
swf_cols <- paste0("d_", swf_argvals)

z_swf <- z |>
  left_join(swf_wide, by = c("id"))

# extract as matrix (rows = observations, cols = SWF distance grid)
swf_matrix <- as.matrix(z_swf[, swf_cols])

# sanity check
dim(swf_matrix)        # should be nrow(z_swf) x length(swf_argvals)
sum(is.na(swf_matrix)) # check for missing joins


m3_swf_functional <- pfr(
  yield_log ~
    lf(swf_matrix,                        # functional SWF effect over distance from border
       argvals = swf_argvals,             # 100, 200, ..., 1000m
       k       = 8) +                     # basis dimension for the coefficient function
    s(log(distance_to_tree_strip), k = 3) +
    s(l_shdi) +
    PC1_s + PC1_c +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") +
    s(year,  bs = "re"),
  data   = z_swf,
  method = "REML"
)

summary(m3_swf_functional)

# coefficient function beta(distance_from_border)
plot(m3_swf_functional, select = 1)  # the lf() term




# refund interaction modell -----------------------------------------------

# create the interaction matrix manually
swf_x_prop <- swf_matrix * z_swf$prop_swf_within   # each row scaled by its scalar

m_int1 <- pfr(
  yield_log ~
    lf(swf_matrix,                                   # main functional effect
       argvals = swf_argvals,
       k       = 8) +
    lf(swf_x_prop,                                   # interaction: pre-multiplied matrix
       argvals = swf_argvals,
       k       = 8) +
    s(log(distance_to_tree_strip), k = 3) +
    s(l_shdi) +
    PC1_s + PC1_c +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") +
    s(year,  bs = "re"),
  data   = z_swf,
  method = "REML"
)


swf_x_dist <- swf_matrix * log(z_swf$distance_to_tree_strip)
swf_x_prop_dist <- swf_matrix * z_swf$prop_swf_within * log(z_swf$distance_to_tree_strip)

m_int2 <- pfr(
  yield_log ~
    lf(swf_matrix,      argvals = swf_argvals, k = 8) +
    lf(swf_x_prop,      argvals = swf_argvals, k = 8) +
    lf(swf_x_prop_dist, argvals = swf_argvals, k = 8) +
    s(log(distance_to_tree_strip), k = 3) +
    s(prop_swf_within) +
    s(l_shdi) +
    PC1_s + PC1_c +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") +
    s(year,  bs = "re"),
  data   = z_swf,
  method = "REML"
)

summary(m_int2)
AIC(m3_swf_functional, m_int1, m_int2)
library(gratia)
draw(m_int1)


# m5 monster model --------------------------------------------------------


m5 <- pfr(
  yield_log ~
    lf(swf_matrix,      argvals = swf_argvals, k = 8) +
    lf(swf_x_prop,      argvals = swf_argvals, k = 8) +
    lf(swf_x_prop_dist, argvals = swf_argvals, k = 8) +
    s(log(distance_to_tree_strip), k = 3) +
    s(prop_swf_within) +
    PC1_l +
    s(fert_N) +
    PC1_s + PC1_c +
    s(crop_unified, distance_to_tree_strip, bs = "re") +
    s(field, bs = "re") +
    s(year,  bs = "re"),
  data   = z_swf,
  method = "REML"
)

summary(m5)


# m6 ----------------------------------------------------------------------

gitcreds::gitcreds_set()
