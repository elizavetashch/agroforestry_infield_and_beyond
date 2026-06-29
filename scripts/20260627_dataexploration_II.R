
# Clean teh Environment
rm(list=ls())


# Packages ----------------------------------------------------------------

library(janitor)
library(dplyr)
library(tidyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(paletteer)


library(tidyverse)   # data wrangling + ggplot2
library(lattice)     # dotplots, xyplots (Zuur's preferred tool)
library(car)         # vif(), scatterplotMatrix()
library(lme4)        # for Step 8 autocorrelation check via random effects
library(performance) # check_normality(), check_collinearity()


# palette -----------------------------------------------------------------

farbenblind_light_contr9 <- c("#77AADD", "#99DDFF", "#44BB99", "#BBCC33","#AAAA00",
                              "#EEDD88", "#EE8866","#FFAABB", "#DDDDDD")
make_palette_graph(farbenblind_light_contr9)

# =============================================================================
# Data structure: Koch 25
#   field: Ihinger Hof 
#   ACS Design  — block, id
#   year        — repeated sampling across time
#   p_dist      — distance to measurement point (blocking factor)
#   yield_wweight — response variable
# =============================================================================

koch25 <- readr::read_delim("data/Koch25/yields_wintercrops.csv", delim = ";", locale = locale(decimal_mark = ","))
koch25$data_id <- "koch25"

koch25 <- janitor::clean_names(koch25)

(nameskoch25 <- names(koch25))

# (1) ACS Design  ---------------------------------------------------------

# 1.1. Year, Block, Id design ---------------------------------------------

df %>% dplyr::summarise(
  n_ids  = dplyr::n_distinct(id),
  n_blocks = dplyr::n_distinct(block),
  n_treatments = dplyr::n_distinct(treatment),
  n_obs  = dplyr::n_distinct(x1),
  .by = year
)

df %>% dplyr::summarise(
  n_obs  = dplyr::n_distinct(x1),
  .by = c(year, block)
)

# 1.2. Design Visualization -----------------------------------------------


design_koch25 <- koch25  %>% 
  distinct(year, block, id, treatment, p_dist, crop, lat, long)

koch25  %>% 
  count(block, id, treatment)  %>% 
  arrange(block, id)

# BLOCK VISUALISATION
ggplot(design_koch25,
       aes(long, lat,
           color = as.factor(block))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = farbenblind_light_contr9) +
  theme_bw() +
  labs(
    title = "Ihinger Hof: Block Design",
    color = "Block Factor",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 5),
    nrow = 1           
  ))

# ID VISUALIZATION
ggplot(design_koch25,
       aes(long, lat, color = as.factor(id))) +
  geom_point(alpha = 0.8) +
  paletteer::scale_color_paletteer_d("ggsci::default_ucscgb") +
  theme_bw() +
  labs(
    title = "Ihinger Hof: ID Design",
    color = "ID",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(override.aes = list(size = 5),
                              nrow = 2)) 


# (2) Investigate Yield Column  -------------------------------------------

hist(koch25$yield_wweight)

# 2.1. Record the unit of yield in the yield_unit column ------------------

koch25$yield_unit <- "t/ha" # known from the publicaiton

# 2.2. yield distribution, check for outliers -----------------------------


# =============================================================================
# Step 1: Are there outliers in Y and X? (Cleveland dotplots)
# =============================================================================
# Dotplots reveal outliers better than boxplots for ecological data (Zuur p. 4)

dotchart(koch25$yield_wweight,
         main = "Dotplot for Yield",
         xlab = "yield_wweight", ylab = "Order of observation")


# (2) Investigate Distance to Tree Row ------------------------------------

boxplot(yield_wweight ~ p_dist, data = koch25,
        main = "Yield per Distance to Tree Row", xlab = "p_dist", ylab = "yield_wweight")


df <- koch25
num <- sapply(df, is.numeric)

Q1 <- apply(df[, num], 2, quantile, 0.25, na.rm = TRUE)
Q3 <- apply(df[, num], 2, quantile, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

outliers <- sweep(df[, num], 2, Q1 - 1.5 * IQR, `<`) |
  sweep(df[, num], 2, Q3 + 1.5 * IQR, `>`)

colSums(outliers)
df[rowSums(outliers) > 0, ]



# (3) Invistigate and document Date and Year ------------------------------

# =============================================================================
# Step 2: Do we have homogeneity of variance?
# =============================================================================
# Conditional boxplots per grouping factor (Zuur Fig. 2)

# Variance by p_dist — this is your primary blocking factor
ggplot(df, aes(x = factor(p_dist), y = yield_wweight)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  labs(title = "Step 2 – Variance homogeneity: yield_wweight ~ p_dist",
       x = "p_dist", y = "yield_wweight") +
  theme_bw()

# Variance by year
ggplot(df, aes(x = factor(year), y = yield_wweight)) +
  geom_boxplot(fill = "darkorange", alpha = 0.6) +
  labs(title = "Step 2 – Variance homogeneity: yield_wweight ~ year",
       x = "year", y = "yield_wweight") +
  theme_bw()

# Variance by block
ggplot(df, aes(x = factor(block), y = yield_wweight)) +
  geom_boxplot(fill = "seagreen", alpha = 0.6) +
  labs(title = "Step 2 – Variance homogeneity: yield_wweight ~ block",
       x = "block", y = "yield_wweight") +
  theme_bw()


# (4) Record crop in the crop column --------------------------------------

levels(as.factor(koch25$crop))
# (5) Record control data -------------------------------------------------

zero_pct <- mean(df$yield_wweight == 0) * 100
cat(sprintf("Zeros in yield_wweight: %.1f%%\n", zero_pct))

# =============================================================================
# Step 3: Are the data normally distributed?
# =============================================================================
# Zuur (p. 7): check Y *and* residuals; histogram + QQ plot

# Overall distribution of yield_wweight
par(mfrow = c(1, 2))
hist(df$yield_wweight, breaks = 20, col = "steelblue",
     main = "Step 3 – Histogram: yield_wweight", xlab = "yield_wweight")
qqnorm(df$yield_wweight, main = "QQ-plot: yield_wweight"); qqline(df$yield_wweight, col = "red")
par(mfrow = c(1, 1))

# Shapiro–Wilk (reliable up to n ≈ 5000; sensitive with large n)
shapiro.test(df$yield_wweight)

# =============================================================================
# Step 4: Zero Inflation
# =============================================================================
zero_pct <- mean(df$yield_wweight == 0) * 100
cat(sprintf("Zeros in yield_wweight: %.1f%% of %d observations\n",
            zero_pct, nrow(df)))

# Zeros by p_dist and year
df |>
  group_by(p_dist, year) |>
  summarise(
    n         = n(),
    n_zeros   = sum(yield_wweight == 0),
    zero_pct  = round(mean(yield_wweight == 0) * 100, 1),
    .groups   = "drop"
  )

# PLOT
df |>
  mutate(is_zero = yield_wweight == 0) |>
  group_by(block, year) |>
  summarise(zero_pct = mean(is_zero) * 100, .groups = "drop") |>
  ggplot(aes(x = factor(year), y = zero_pct, fill = factor(block))) +
  geom_col(position = "dodge") +
  labs(title  = "Zero proportion by block × year",
       x      = "year",
       y      = "% zeros",
       fill   = "block") +
  theme_bw()


# Organize and rename  ----------------------------------------------------

koch25$x1 <- NULL
names(koch25)[names(koch25) == "p_dist"] <- "distance_to_tree_row"
names(koch25)[names(koch25) == "yield_wweight"] <- "yield"
(nameskoch25 <- names(koch25))

write.csv(koch25, file = "data/AnalysisData/20260629_ihingerhof.csv", row.names = FALSE)

# =============================================================================
# Data structure: Koch 25
#   field: Wendhausen
#   ACS Design  — 
#   year        — 
#   distance to tree row     — "distance_to_tree_strip"
#   yield — "or_dm"    "or_dm_content"  "or_1000seed" "or_crude_fat" "ww_dm" "ww_dm_content" "ww_1000seed" 
            "ww_crude_protein"       "decomposition"          "litter_dm"              "wood_yield_estimated"
#   coordinates - lat and lon
# =============================================================================


# Wendhausen  -------------------------------------------------------------

# Wendhausen 15 18 --------------------------------------------------------


wendhausen_1518 <- read_csv("data/BONARES_Cropland agroforestry 2015-2018/SIGNAL.ID_7013_DATEN_WH_15_18.csv")
wendhausen_1518$data_id <- "wendhausen1518"

# NAMES
(names(wendhausen_1518) <- names(wendhausen_1518))
wendhausen_1518 <- janitor::clean_names(wendhausen_1518)
(names(wendhausen_1518) <- names(wendhausen_1518))


# CROP AND YIELD 
# keep only the dm columns
# in year 2016 only wood was harvested
wendhausen_1518 <- 
  wendhausen_1518 %>% 
  select(year, lat, long, distance_to_tree_row, orientation, soil_type, or_dm, ww_dm, plot, data_id ) %>% 
  filter(year != 2015) 

# YEAR
# 2015 only wood
# 2016 or
# 2017 ww
# 2018 wood 


wendhausen_1518 <- wendhausen_1518 %>%
  mutate(
    crop = case_when(
      year == 2016 ~ "oil rape",
      year == 2017 ~ "winter wheat",
      TRUE ~ NA_character_
    ),
    yield = coalesce(or_dm, ww_dm)
  ) %>%
  select(-or_dm, -ww_dm)

write.csv(wendhausen_1518, file = "data/AnalysisData/20260629_wendhausen1518.csv", row.names = FALSE)

# Wendhausen 17 18 --------------------------------------------------------

wendhausen_1718 <- read_csv("data/BONARES_Cropland Agroforestry 2017 and 2018/signal.ID_7042_BIOMASSE_17_18_WH_280319.csv")
wendhausen_1718$wendhausen_1718 <- "wendhausen1718"
(names(wendhausen_1718) <- names(wendhausen_1718))

wendhausen_1718 <- janitor::clean_names(wendhausen_1718)

straw <- wendhausen_1718 %>%
  select(plot, year, straw_dm) %>%
  filter(!is.na(straw_dm)) %>%
  mutate(
    crop = "straw",
    yield = straw_dm
  ) %>%
  select(plot, year, crop, yield)

wendhausen_1518 <- bind_rows(wendhausen_1518,straw)

write.csv(wendhausen_1518, file = "data/AnalysisData/20260629_wendhausen1518.csv", row.names = FALSE)

# Wendhausen 19 20 --------------------------------------------------------


wendhausen_1920 <- read_csv("data/BONARES_Cropland agroforestry 2019-2020/signal.ID_7069_DATEN_WENDHAUSEN_2019_20.csv")
wendhausen_1920$data_id <- "wendhausen1920"
wendhausen_1920 <- janitor::clean_names(wendhausen_1920)
names(wendhausen_1920)


wendhausen_1920 <- wendhausen_1920[, -(10:15)]
wendhausen_1920$crop = "silage maize"
names(wendhausen_1920)[names(wendhausen_1920) == "sm_dm"] <- "yield"
wendhausen_1520 <- bind_rows(wendhausen_1518,wendhausen_1920)

# Wendhausen 21 --------------------------------------------------------

wendhausen_21 <- read_csv("data/BONARES_Cropland agroforestry 2021/signal.ID_7070_DATEN_WENDHAUSEN_2021.csv")
wendhausen_21$data_id <- "wendhausen21"
wendhausen_21 <- janitor::clean_names(wendhausen_21)
names(wendhausen_21)

wendhausen_21 <- wendhausen_21[, -(10:15)]
wendhausen_21$crop = "summer barley"
names(wendhausen_21)[names(wendhausen_21) == "sb_dm"] <- "yield"
wendhausen_1521 <- bind_rows(wendhausen_1520,wendhausen_21)

# Wendhausen 21 --------------------------------------------------------

wendhausen_22 <- read_csv("data/BONARES_Cropland agroforestry 2022/signal.ID_7071_DATEN_WENDHAUSEN_2022.csv")
wendhausen_22$data_id <- "wendhausen22"

wendhausen_23 <- read_csv("data/BONARES_Cropland agroforestry 2023/signal.ID_7077_DATEN_WENDHAUSEN_2023_CR.csv")
wendhausen_23$data_id <- "wendhausen23"


# ORGANIZE AND RENAME
names(wendhausen_1518)[names(wendhausen_1518) == "distance_to_tree_strip"] <- "distance_to_tree_row"
names(wendhausen_1518)[names(wendhausen_1518) == "lon"] <- "long"

