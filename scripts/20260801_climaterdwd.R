

rm(list=ls())

library(tidyverse)
library(dplyr)
library(rdwd)
library(stringr)
rdwd::updateRdwd()

# my data 
dfanalysis <-  read.csv("data/AnalysisData/20260801_dfanalysis.csv")
dfanalysis <- dfanalysis %>%
  mutate(
    gs_start_date = as.Date(gs_start_date, format = "%Y-%m-%d"),
    gs_end_date   = as.Date(gs_end_date, format = "%Y-%m-%d")
  )

fields <- dfanalysis %>%
  select(field, latitude, longitude) %>%
  unique()

# dwd data 
data("gridIndex")

(precip <- grep("monthly/precipitation", gridIndex, value=TRUE)) 
(temp <- grep("monthly/air_temperature_mean", gridIndex, value=TRUE))
#(index <- grep("monthly/soil_temperature", gridIndex, value=TRUE))
(sun <- grep("monthly/sun", gridIndex, value=TRUE)) 

(precip <- grep('2015|2016|2017|2018|2019|2020|2021|2022|2023',precip, value=TRUE))
(temp <- grep('2015|2016|2017|2018|2019|2020|2021|2022|2023',temp, value=TRUE))
(sun <- grep('2015|2016|2017|2018|2019|2020|2021|2022|2023',sun, value=TRUE))

rprecip <- dataDWD(precip, base=gridbase, joinbf=TRUE)
rtemp <- dataDWD(temp, base=gridbase, joinbf=TRUE)
rsun <- dataDWD(sun, base=gridbase, joinbf=TRUE)

y <- fields$latitude
x <- fields$longitude
fieldname <- fields$field

loc <- data.frame(x=x, y=y) 


# sun terra raster:
rsun_stack <- terra::rast(rsun)
rsun_stack <- projectRasterDWD(rsun_stack, proj="seasonal", extent="seasonal")
rowid <- c(1:864)
suntable <- terra::extract(rsun_stack, loc)[-1]
names(suntable) <- format(as.Date(paste0(rep(2015:2023, times = 12), "-", rep(sprintf("%02d", 1:12), each = 9), "-01")), "%Y-%m-%d")
suntable$field <- fieldname

suntable <- suntable %>% 
  pivot_longer( cols = -field, names_to = "date", values_to = "sun__MJ_m2" ) %>% 
  mutate( date = as.Date(date), year = format(date, "%Y"), month = format(date, "%m") )

write.csv(suntable, "data/climate/rdwd/20260801_suntable.csv", row.names = FALSE)

# percip terra raster:
precip_stack <- terra::rast(rprecip)
precip_stack <- projectRasterDWD(precip_stack, proj="seasonal", extent="seasonal")
preciptable <- terra::extract(precip_stack, loc)[-1]
names(preciptable) <- format(as.Date(paste0(rep(2015:2023, times = 12), "-", rep(sprintf("%02d", 1:12), each = 9), "-01")), "%Y-%m-%d")
preciptable$field <- fieldname

preciptable <- preciptable %>% 
  pivot_longer( cols = -field, names_to = "date", values_to = "prec_mm" ) %>% 
  mutate( date = as.Date(date), year = format(date, "%Y"), month = format(date, "%m") )

write.csv(preciptable, "data/climate/rdwd/20260801_preciptable.csv", row.names = FALSE)
# temp terra raster:
temp_stack <- terra::rast(rtemp)
temp_stack <- projectRasterDWD(temp_stack, proj="seasonal", extent="seasonal")
temptable <- terra::extract(temp_stack, loc)[-1]
names(temptable) <- format(as.Date(paste0(rep(2015:2023, times = 12), "-", rep(sprintf("%02d", 1:12), each = 9), "-01")), "%Y-%m-%d")
temptable$field <- fieldname

temptable <- temptable %>% 
  pivot_longer( cols = -field, names_to = "date", values_to = "temp_C" ) %>% 
  mutate( date = as.Date(date), year = format(date, "%Y"), month = format(date, "%m") )

write.csv(temptable, "data/climate/rdwd/20260801_temptable.csv", row.names = FALSE)


# ooo ---------------------------------------------------------------------


temptable <- read.csv("data/climate/rdwd/20260801_temptable.csv")
preciptable <- read.csv("data/climate/rdwd/20260801_preciptable.csv")
suntable <- read.csv("data/climate/rdwd/20260801_suntable.csv")

temptable <- temptable |> mutate(date = as.Date(date, format = "%Y-%m-%d"))
preciptable <- preciptable |> mutate(date = as.Date(date, format = "%Y-%m-%d"))
suntable <- suntable |> mutate(date = as.Date(date, format = "%Y-%m-%d"))


result <- dfanalysis %>%
  rowwise() %>%
  mutate(
    # temperature
    temp_C_mean = mean(
      temptable$temp_C[
        temptable$field == field &
          temptable$date >= gs_start_date &
          temptable$date <= gs_end_date
      ],
      na.rm = TRUE
    ),
    # radiation
    sun_MJ_m2_mean = mean(
      suntable$sun__MJ_m2[
        suntable$field == field &
          suntable$date >= gs_start_date &
          suntable$date <= gs_end_date
      ],
      na.rm = TRUE
    ),
    # precipitation
    precip_mm_sum = sum(
      preciptable$prec_mm[
        preciptable$field == field &
          preciptable$date >= gs_start_date &
          preciptable$date <= gs_end_date
      ],
      na.rm = TRUE
    )
    
  ) %>%
  ungroup()

write.csv(result, "data/AnalysisData/20260801_AFclimate.csv", row.names = FALSE)


# swf ---------------------------------------------------------------------

df <- read.csv("data/AnalysisData/20260801_AFclimate.csv")
df$area |> unique()

library(geosphere)
df <- df %>% 
  mutate( 
    fieldlength = distHaversine( cbind(min_longitude, min_latitude), cbind(max_longitude, max_latitude) ) ) 

write.csv(df, "data/AnalysisData/20260803_AFdistance.csv", row.names = FALSE)


library(ggplot2)
library(car)
library(car)
library(lme4)

df <- read.csv("data/AnalysisData/20260801_AFclimate.csv")
dfmodnomaize <- df |> filter(crop_unified != "maize")

mod <- glm(yield_tha ~ temp_C_mean*sun_MJ_m2_mean*precip_mm_sum+ crop_unified, data = dfmodnomaize, family = "gaussian")

par(mfrow = c(2,2))
plot(mod)


Anova(mod, type = "II", test.statistic = "F")
summary(mod)
out.anova <- Anova(mod, test.statistic = "F")    
R2.value <- sum(out.anova[,"Sum Sq"][1:nrow(out.anova)-1]) / sum(out.anova[,"Sum Sq"])
R2.value

mod <- lmer(
  yield_tha ~ distance_to_tree_strip*treeage + crop_unified +
    (1 | field) + (1 | year), data = dfmodnomaize)


sjPlot::plot_model(mod)
sjPlot:: tab_model(mod)
summary(mod)
