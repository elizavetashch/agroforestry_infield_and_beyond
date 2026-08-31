
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(refund)
library(mgcv)

rm(list=ls())


df <- read.csv("C:\\Users\\Elizaveta/OneDrive - Universität Bayreuth\\Dokumente\\MasterThesis\\MA_RProject\\data\\AnalysisData\\20260831_full.csv")

# yield log
df2 <- df |> mutate(yield_log = log(yield_tha), yield_scaled = as.numeric(scale(yield_log)))



# swf function ------------------------------------------------------------

# data swf
swf <- df2 |> 
  distinct(ID_nodist, field,swf_year, distance, prop_swf)  |> 
  pivot_wider(
    names_from = distance,
    values_from = prop_swf
  ) |> 
  arrange(ID_nodist)
# co variates
grid <- as.numeric(names(swf)[-(1:3)])
Y_swf <- as.matrix(swf[, -(1:3)])
field_id <- factor(swf$field)
swf_year <- factor(swf$swf_year)
id_order <- swf$ID_nodist
#checks
dim(Y_swf)
length(grid)
length(field_id)
length(swf_year)



# swf model ---------------------------------------------------------------

swf_m <- pffr(
  Y_swf ~ 
    field_id +
    s(swf_year, bs = "re"),
  yind = grid,
  data = list(Y_swf = Y_swf, 
              field_id = factor(field_id),
              swf_year= factor(swf_year)),
  family = binomial()
)

plot(swf_m)

swf_m2 <- pffr(
  Y_swf ~ 
    s(field_id, bs = "re") +
    s(swf_year, bs = "re"),
  yind = grid,
  data = list(Y_swf = Y_swf, 
              field_id = factor(field_id),
              swf_year= factor(swf_year)),
  family = binomial()
)


# swf  fitted values ---------------------------------------------

# swf with fixed fields
fitted_swf <- fitted(swf_m)
rownames(fitted_swf) <- swf$ID_nodist
W_swf <- fitted_swf[match(df3$ID_nodist, rownames(fitted_swf)),]
df3$W_swf <- W_swf
all(df3$ID_nodist %in% swf$ID_nodist)
dim(W_swf)

matplot(
  grid,
  t(W_swf),
  type = "l",
  lty = 1,
  xlab = "Distance",
  ylab = "Predicted SWF"
)
# swf with random fields
fitted_swf2 <- fitted(swf_m2)
rownames(fitted_swf2) <- swf$ID_nodist
W_swf2 <- fitted_swf2[match(df3$ID_nodist, rownames(fitted_swf2)),]
df3$W_swf2 <- W_swf2
all(df3$ID_nodist %in% swf$ID_nodist)
dim(W_swf2)

matplot(
  grid,
  t(W_swf2),
  type = "l",
  lty = 1,
  xlab = "Distance",
  ylab = "Predicted SWF"
)


# yield function ----------------------------------------------------------

yield_grid <- sort(unique(df2$distance_to_tree_strip))

df3 <- df2  |> 
  distinct(ID_nodist, field, year, crop_unified,
    fert_N, treeage, AFage,prop_swf_within,
    l_shdi, l_contag,l_ed,
    temp_C_mean, clay,mean_slope,max_slope,
    distance_to_tree_strip,
    yield_log
  ) |> 
  pivot_wider(
    names_from = distance_to_tree_strip,
    values_from = yield_log
  )  |> 
  arrange(ID_nodist) |> 
  mutate(field = as.factor(field), 
         crop_unified = as.factor(crop_unified),
         year = as.factor(year))

Y <- df3  |> 
  select(all_of(as.character(yield_grid))) %>%
  as.matrix()



# pffr function -----------------------------------------------------------

# m1 --------------
yield_m1 <- pffr(
  Y ~ ff(W_swf, xind = grid),
  yind = yield_grid,
  data = df3
)
summary(yield_m1)
plot(yield_m1,pages = 1)

# m2 --------------------
yield_m2 <- pffr(
  Y ~ ff(W_swf, xind = grid) +
    s(field_id, bs = "re") +
    year,
  yind = yield_grid,
  data = df3
)
# m3 random fields --------------------
yield_m3 <- pffr(
  Y ~ ff(W_swf2, xind = grid) +
    s(field_id, bs = "re") +
    year,
  yind = yield_grid,
  data = df3
)

summary(yield_m3)
plot(yield_m3,pages = 1)

# m4 random fields and rand year--------------------
yield_m4 <- pffr(
  Y ~ ff(W_swf2, xind = grid) +
    s(field_id, bs = "re") +
    s(year, bs = "re"),
  yind = yield_grid,
  data = df3
)

summary(yield_m4)
plot(yield_m4,pages = 1)

# m5 observed swf --------------------

yield_swfobs_m1 <- pffr(
  Y ~ ff(Y_swf, xind = grid),
  yind = yield_grid,
  data = df3
)

summary(yield_swfobs_m1)
plot(yield_swfobs_m1, pages = 1)
# m6 observed swf --------------------

yield_swfobs_m2 <- pffr(
  Y ~ ff(Y_swf, xind = grid) +
    s(field_id, bs = "re") +
    s(year, bs = "re") +
    crop_unified,
  yind = yield_grid,
  sandwich = "none",
  data = df3
)

summary(yield_swfobs_m2)
plot(yield_swfobs_m2)
AIC(yield_swfobs_m2)

yield_swfobs_m3 <- pffr(
  Y ~ ff(Y_swf, xind = grid) +
    s(field_id, bs = "re") +
    s(year, bs = "re") +
    crop_unified +
    fert_N +
    clay+ 
    l_ed + 
    temp_C_mean + 
    mean_slope,
  yind = yield_grid,
  sandwich = "none",
  data = df3
)

summary(yield_swfobs_m3)
plot(yield_swfobs_m3)

AIC(yield_swfobs_m3)





# up until there done on the 0830 -----------------------------------------























# -------------------------------------------------------------------------

# OLD ---------------------------------------------------------------------



# widen format 
data_wide <- df2 |> 
  pivot_wider(
    names_from = distance_to_tree_strip,
    values_from = yield_log
  ) |> 
  arrange(ID_nodist) |> 
  as.data.frame()

nrow(data_wide) # 11710

# 
df3 <- df2 |> 
  distinct(ID_nodist, temp_C_mean, field, year, yield_log, distance_to_tree_strip, # field
           crop_unified, fert_N, treeage, AFage, prop_swf_within, # field design 
           l_shdi, l_contag, l_ed, # landcover metrics
           temp_C_mean, clay, mean_slope, max_slope)|> 
  pivot_wider(
    names_from = distance_to_tree_strip,
    values_from = yield_log
  ) |> 
  arrange(ID_nodist)

# swf predictor matrix
swf <- data_wide |>
  dplyr::select(ID_nodist, field, swf_year, distance, prop_swf) |>
  unique() |>
  tidyr::pivot_wider(names_from = distance, values_from = prop_swf) |>
  arrange(ID_nodist)

id_order <- swf$ID_nodist
#W_mat    <- as.matrix(W_swf[, -c(1,2)])
#grid     <- as.numeric(colnames(W_mat))   # 100 200 ... 1000

grid <- as.numeric(names(swf)[-c(1:3)])
Y_swf <- as.matrix(swf[, -c(1:3)])
field_id <- swf |> distinct(ID_nodist, field) |> arrange(ID_nodist) |> pull(field)
swf_year <- swf |> pull(swf_year)

swf_m1 <- pffr(
  Y_swf ~ 
    s(field_id, bs = "re") +
    s(swf_year, bs = "re"),
  yind = grid,
  data = list(Y_swf = Y_swf, 
              field_id = factor(field_id),
              swf_year= factor(swf_year)),
  family = binomial()
)

swf_m2 <- pffr(
  Y_swf ~ 
    s(field_id, bs = "re") +
    swf_year,
  yind = grid,
  data = list(Y_swf = Y_swf, 
              field_id = factor(field_id),
              swf_year= factor(swf_year)),
  family = binomial()
)

swf_m3 <- pffr(
  Y_swf ~ 
    field_id +
    swf_year,
  yind = grid,
  data = list(Y_swf = Y_swf, 
              field_id = factor(field_id),
              swf_year= factor(swf_year)),
  family = binomial()
)

swf_m4 <- pffr(
  Y_swf ~ 
    field_id +
    s(swf_year, bs = "re"),
  yind = grid,
  data = list(Y_swf = Y_swf, 
              field_id = factor(field_id),
              swf_year= factor(swf_year)),
  family = binomial()
)

summary(swf_m1) # Best 
summary(swf_m2)
summary(swf_m3)
summary(swf_m4)

plot(swf_m4)

fitted_swf <- fitted(swf_m4)
rownames(fitted_swf) <- swf$ID_nodist

W_smooth <- fitted_swf[match(id_order, rownames(fitted_swf)), ]

df3$W_swf <- W_smooth


# response matrix
Y <- df3 %>% select(as.character(sort(unique(df2$distance_to_tree_strip)))) %>% as.matrix()
nrow(Y)



# model
# Distance grid
t <- sort(unique(df2$distance_to_tree_strip))

# Fit model
m <- pffr(Y ~ 
            ff(W_swf, xind=grid),
          yind = t,
          data = df3)
summary(m)
plot(m, pages=1)
