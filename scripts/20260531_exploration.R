

library(readr)
library(dplyr)


# Koch 2025 ---------------------------------------------------------------


yields_wintercrops <- read_csv("data/Koch25/yields_wintercrops.csv")
yields_wintercrops <- read_delim("data/Koch25/yields_wintercrops.csv", delim = ";")

# 24 IDs : Germany 


# Paut 2023 ---------------------------------------------------------------

paut23 <- read_delim("data/Paut23/1. Database.csv", delim = ";")
View(paut23)


europe_countries <- c(
  "Albania", "Andorra", "Austria", "Belarus", "Belgium",
  "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus",
  "Czech Republic", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy",
  "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg",
  "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands",
  "North Macedonia", "Norway", "Poland", "Portugal", "Romania",
  "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain",
  "Sweden", "Switzerland", "Ukraine", "United Kingdom",
  "Vatican City"
)

# filter to EU
paut23_eu <- paut23_europe <- paut23 %>%
  filter(Country %in% europe_countries)

# Agroforestry yes
paut23_euAF <- paut23_eu %>% 
  filter(AF == "yes")

# 8 observations  : UK, France, Croatia, Italy 

datasets <- list(
  koch25 = yields_wintercrops,
  paut23 = paut23_euAF
)

# Baier 2023 --------------------------------------------------------------

library(readxl)
library(tidyr)
library(sf)
library(rnaturalearth)
library(terra)
library(stringr)

baier23 <- read_excel("data/Baier23/Effects of Agroforestry on Grain Yield of Maize_Main Dataset.xlsx")

baier23 <- baier23 %>%
  rename(Latitude_Decimal = `Latitude Decimal`)%>%
  rename(Longitude_Decimal = `Longitude Decimal`)

baier23 <- baier23 %>% drop_na(Longitude_Decimal)

points_sf <- st_as_sf(baier23, coords = c("Longitude_Decimal", "Latitude_Decimal"), crs = 4326)
world <- ne_countries(scale = "medium", returnclass = "sf")
europe <- world %>% filter(continent == "Europe")

points_europe <- st_join(points_sf, europe, join = st_within) %>%
  filter(!is.na(continent))

germany <- world %>% filter(admin == "Germany")

points_germany <- st_join(points_sf, germany, join = st_within) %>%
  filter(!is.na(admin))

baier23_selection <- points_europe %>% 
  filter(`Year of Publication` == 2019)
baier23_selection <- as_tibble(baier23_selection)

# append to the list 
datasets <- append(datasets, list(baier23 = baier23_selection))


# BONARES Data ------------------------------------------------------------

wendhausen_1518 <- read_csv("data/BONARES_Cropland agroforestry 2015-2018/SIGNAL.ID_7013_DATEN_WH_15_18.csv")
wendhausen_1718 <- read_csv("data/BONARES_Cropland Agroforestry 2017 and 2018/signal.ID_7042_BIOMASSE_17_18_WH_280319.csv")
wendhausen_1920 <- read_csv("data/BONARES_Cropland agroforestry 2019-2020/signal.ID_7069_DATEN_WENDHAUSEN_2019_20.csv")
wendhausen_21 <- read_csv("data/BONARES_Cropland agroforestry 2021/signal.ID_7070_DATEN_WENDHAUSEN_2021.csv")
wendhausen_22 <- read_csv("data/BONARES_Cropland agroforestry 2022/signal.ID_7071_DATEN_WENDHAUSEN_2022.csv")
wendhausen_23 <- read_csv("data/BONARES_Cropland agroforestry 2023/signal.ID_7077_DATEN_WENDHAUSEN_2023_CR.csv")
dornburg_16 <- read_csv("data/Bonares_Dornburg/signal.ID_7004_PROD_D_2016_V2.csv")
reiffenhausen_1617 <- read_csv("data/BONARES_Reiffenhausen/signal.ID_7039_REIFFENHAUSEN_BIOMASS_DATA_V2.csv")



# Append and Save ---------------------------------------------------------


datasets <- append(datasets, list(wendhausen_1518 = wendhausen_1518,
                                  wendhausen_1718 = wendhausen_1718,
                                  wendhausen_1920 = wendhausen_1920,
                                  wendhausen_21 = wendhausen_21, 
                                  wendhausen_22 = wendhausen_22, 
                                  wendhausen_23 = wendhausen_23, 
                                  dornburg_16 = dornburg_16, 
                                  reiffenhausen_1617 = reiffenhausen_1617))



# Gladbacherhof ---------------------------------------------------------

gladbacherhof <- read_csv("data/Gladbacherhof21/yield.csv")
datasets <- append(datasets, list(gladbacherhof))

# save the data 
saveRDS(datasets, "data/datasets_list.rds")


# Hessen ZALF -------------------------------------------------------------


hessen2122 <- read_excel("data/ZALF_Hessen_2122/AFGH1_Yield_ALL.xlsx")
datasets <- append(datasets, list(hessen2122 = hessen2122))


# Mariensee1517 --------------------------------------------------------------------

mariensee1517 <- read_csv("data/Mariensee1517/signal.ID_7008_Gras_Laub_Holz_MS_2015_2016_2017.csv")

datasets <- append(datasets, list(mariensee1517 = mariensee1517))


# -------------------------------------------------------------------------
###########################################################################
#                        SINGLE DATASET EXPLORATION                       #
###########################################################################
# -------------------------------------------------------------------------



# BONARES Wendhausen ------------------------------------------------------

wendhausen <- list(wendhausen_1518, wendhausen_1718, wendhausen_1920, wendhausen_21, wendhausen_22, wendhausen_23)

purrr::walk(wendhausen, glimpse)

summary(wendhausen_1518)



# =============================================================================
# WENDHAUSEN 1518 - Comprehensive Exploratory Data Analysis
# Agroforestry Strip Dataset (2015-2018)
# =============================================================================

# --- 1. SETUP & LIBRARIES ----------------------------------------------------

# Install missing packages if needed
packages <- c("ggplot2", "dplyr", "tidyr", "corrplot", "naniar", "ggcorrplot",
              "patchwork", "scales", "ggridges", "viridis", "GGally",
              "knitr", "moments", "gridExtra", "RColorBrewer")

installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]
if (length(to_install) > 0) install.packages(to_install)

lapply(packages, library, character.only = TRUE)


# --- 2. LOAD DATA ------------------------------------------------------------
# Adjust path as needed
# df <- read.csv("wendhausen_1518.csv", stringsAsFactors = FALSE)
# OR if it's an .RData / .rds:
# df <- readRDS("wendhausen_1518.rds")

# For demonstration, assuming the object is already in the environment:
df <- wendhausen_1518


# =============================================================================
# SECTION 1: DATA OVERVIEW
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  WENDHAUSEN 1518 — DATA OVERVIEW\n")
cat(strrep("=", 70), "\n\n")

cat("Dimensions:", nrow(df), "rows x", ncol(df), "columns\n\n")
cat("Column names:\n")
print(names(df))

cat("\nData types:\n")
str(df)

cat("\nFull summary:\n")
print(summary(df))


# =============================================================================
# SECTION 2: MISSING VALUE ANALYSIS
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 2: MISSING VALUES\n")
cat(strrep("=", 70), "\n\n")

# --- 2a. Missing count table -------------------------------------------------
missing_df <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "N_Missing") %>%
  mutate(
    Pct_Missing = round(N_Missing / nrow(df) * 100, 1),
    Complete    = nrow(df) - N_Missing
  ) %>%
  arrange(desc(N_Missing))

cat("Missing value counts:\n")
print(missing_df, n = Inf)

# --- 2b. Missing bar chart ---------------------------------------------------
p_missing <- missing_df %>%
  filter(N_Missing > 0) %>%
  ggplot(aes(x = reorder(Variable, N_Missing), y = Pct_Missing, fill = Pct_Missing)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(Pct_Missing, "%\n(n=", N_Missing, ")")),
            hjust = -0.05, size = 3, lineheight = 0.85) +
  coord_flip() +
  scale_fill_gradient(low = "#f7c59f", high = "#c0392b", name = "% Missing") +
  scale_y_continuous(limits = c(0, 110), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Missing Values by Variable",
    subtitle = paste("Total rows:", nrow(df)),
    x        = NULL,
    y        = "% Missing"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

print(p_missing)

# --- 2c. naniar upset / heatmap (if naniar available) -----------------------
if (requireNamespace("naniar", quietly = TRUE)) {
  p_vis_miss <- naniar::vis_miss(df, warn_large_data = FALSE) +
    labs(title = "Missing Data Map (all rows × all columns)") +
    theme(plot.title = element_text(face = "bold"))
  print(p_vis_miss)
}


# =============================================================================
# SECTION 3: DISTRIBUTIONS — NUMERIC VARIABLES
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 3: DISTRIBUTIONS — NUMERIC VARIABLES\n")
cat(strrep("=", 70), "\n\n")

numeric_vars <- df %>% select(where(is.numeric)) %>% names()
cat("Numeric variables:", paste(numeric_vars, collapse = ", "), "\n\n")

# --- 3a. Descriptive statistics table ----------------------------------------
desc_stats <- df %>%
  select(all_of(numeric_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    N        = sum(!is.na(Value)),
    Mean     = round(mean(Value, na.rm = TRUE), 4),
    SD       = round(sd(Value,   na.rm = TRUE), 4),
    Min      = round(min(Value,  na.rm = TRUE), 4),
    Q1       = round(quantile(Value, 0.25, na.rm = TRUE), 4),
    Median   = round(median(Value, na.rm = TRUE), 4),
    Q3       = round(quantile(Value, 0.75, na.rm = TRUE), 4),
    Max      = round(max(Value,  na.rm = TRUE), 4),
    Skewness = round(moments::skewness(Value, na.rm = TRUE), 3),
    Kurtosis = round(moments::kurtosis(Value, na.rm = TRUE), 3),
    .groups  = "drop"
  )

cat("Descriptive statistics:\n")
print(desc_stats, n = Inf)

# --- 3b. Histograms (faceted) ------------------------------------------------
df_long_num <- df %>%
  select(all_of(numeric_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

p_hist <- ggplot(df_long_num, aes(x = Value, fill = Variable)) +
  geom_histogram(bins = 15, color = "white", alpha = 0.85) +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title    = "Distribution of All Numeric Variables",
    subtitle = "Histograms with free scales",
    x = NULL, y = "Count"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position  = "none",
    strip.text       = element_text(face = "bold", size = 9),
    plot.title       = element_text(face = "bold", size = 13)
  )

print(p_hist)

# --- 3c. Density plots -------------------------------------------------------
p_density <- ggplot(df_long_num, aes(x = Value, fill = Variable, color = Variable)) +
  geom_density(alpha = 0.4, linewidth = 0.7) +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  scale_fill_viridis_d(option = "mako") +
  scale_color_viridis_d(option = "mako") +
  labs(
    title    = "Density Plots — Numeric Variables",
    x = NULL, y = "Density"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    strip.text      = element_text(face = "bold", size = 9),
    plot.title      = element_text(face = "bold", size = 13)
  )

print(p_density)

# --- 3d. QQ plots (normality check) -----------------------------------------
# Select key agronomic variables for QQ
key_vars <- intersect(
  c("OR_DM", "OR_DM_CONTENT", "OR_1000SEED", "OR_CRUDE_FAT",
    "WW_DM", "WW_DM_CONTENT", "WW_1000SEED", "WW_CRUDE_PROTEIN",
    "Straw_DM", "Wheat_DM", "Wheat_1000seed", "Wheat_crude_protein",
    "Litter_DM", "LITTER_DM", "DECOMPOSITION", "Distance_to_tree_strip",
    "DISTANCE_TO_TREE_STRIP"),
  numeric_vars
)

if (length(key_vars) >= 1) {
  par(mfrow = c(ceiling(length(key_vars) / 4), 4), mar = c(3, 3, 2, 1))
  for (v in key_vars) {
    vals <- df[[v]][!is.na(df[[v]])]
    if (length(vals) > 3) {
      qqnorm(vals, main = v, col = "#2c7bb6", pch = 16, cex = 0.7)
      qqline(vals, col = "#d7191c", lwd = 1.5)
    }
  }
  par(mfrow = c(1, 1))
}


# =============================================================================
# SECTION 4: BOXPLOTS & OUTLIER DETECTION
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 4: BOXPLOTS & OUTLIER DETECTION\n")
cat(strrep("=", 70), "\n\n")

# --- 4a. Boxplots (normalised for side-by-side display) ----------------------
df_scaled <- df %>%
  select(all_of(numeric_vars)) %>%
  mutate(across(everything(), ~ as.numeric(scale(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Z_Score")

p_box_all <- ggplot(df_scaled, aes(x = reorder(Variable, Z_Score, FUN = median, na.rm = TRUE),
                                   y = Z_Score, fill = Variable)) +
  geom_boxplot(outlier.color = "#c0392b", outlier.shape = 21,
               outlier.fill = "#e74c3c", outlier.size = 2, alpha = 0.8) +
  coord_flip() +
  scale_fill_viridis_d(option = "turbo", alpha = 0.8) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title    = "Boxplots of All Numeric Variables (Z-scored)",
    subtitle = "Red dashed lines = ±3 SD threshold",
    x = NULL, y = "Z-Score"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold", size = 13)
  )

print(p_box_all)

# --- 4b. IQR-based outlier table ---------------------------------------------
outlier_summary <- df %>%
  select(all_of(numeric_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Q1      = quantile(Value, 0.25, na.rm = TRUE),
    Q3      = quantile(Value, 0.75, na.rm = TRUE),
    IQR     = IQR(Value, na.rm = TRUE),
    Lower   = Q1 - 1.5 * IQR(Value, na.rm = TRUE),
    Upper   = Q3 + 1.5 * IQR(Value, na.rm = TRUE),
    N_Low   = sum(Value < Q1 - 1.5 * IQR(Value, na.rm = TRUE), na.rm = TRUE),
    N_High  = sum(Value > Q3 + 1.5 * IQR(Value, na.rm = TRUE), na.rm = TRUE),
    N_Total = sum(N_Low, N_High),
    .groups = "drop"
  ) %>%
  arrange(desc(N_Total))

cat("IQR-based outlier counts per variable:\n")
print(outlier_summary, n = Inf)

# --- 4c. Individual boxplots grouped by Year / Orientation (where applicable)
year_col <- if ("Year" %in% names(df)) "Year" else if ("YEAR_" %in% names(df)) "YEAR_" else NULL
ori_col  <- if ("Orientation" %in% names(df)) "Orientation" else if ("ORIENTATION" %in% names(df)) "ORIENTATION" else NULL

# Yield variables of interest
yield_vars <- intersect(
  c("OR_DM", "WW_DM", "Wheat_DM", "Straw_DM",
    "OR_1000SEED", "WW_1000SEED", "Wheat_1000seed"),
  names(df)
)

if (!is.null(year_col) && length(yield_vars) >= 1) {
  df_yield_long <- df %>%
    select(all_of(c(year_col, yield_vars))) %>%
    rename(Year = all_of(year_col)) %>%
    mutate(Year = factor(Year)) %>%
    pivot_longer(-Year, names_to = "Variable", values_to = "Value")
  
  p_box_year <- ggplot(df_yield_long, aes(x = Year, y = Value, fill = Year)) +
    geom_boxplot(outlier.color = "#c0392b", alpha = 0.8) +
    facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title    = "Yield Variables by Year",
      x = "Year", y = "Value"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      strip.text      = element_text(face = "bold"),
      plot.title      = element_text(face = "bold", size = 13)
    )
  print(p_box_year)
}

if (!is.null(ori_col) && length(yield_vars) >= 1) {
  df_yield_ori <- df %>%
    select(all_of(c(ori_col, yield_vars))) %>%
    rename(Orientation = all_of(ori_col)) %>%
    pivot_longer(-Orientation, names_to = "Variable", values_to = "Value")
  
  p_box_ori <- ggplot(df_yield_ori, aes(x = Orientation, y = Value, fill = Orientation)) +
    geom_boxplot(outlier.color = "#c0392b", alpha = 0.8) +
    facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
    scale_fill_brewer(palette = "Pastel1") +
    labs(
      title    = "Yield Variables by Tree Strip Orientation",
      x = "Orientation", y = "Value"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text      = element_text(face = "bold"),
      plot.title      = element_text(face = "bold", size = 13)
    )
  print(p_box_ori)
}


# =============================================================================
# SECTION 5: BAR CHARTS — CATEGORICAL & COUNT SUMMARIES
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 5: BAR CHARTS\n")
cat(strrep("=", 70), "\n\n")

cat_vars <- df %>% select(where(is.character)) %>% names()
cat("Categorical variables:", paste(cat_vars, collapse = ", "), "\n")

# --- 5a. Frequency bar charts for each categorical --------------------------
for (v in cat_vars) {
  p <- df %>%
    count(.data[[v]]) %>%
    ggplot(aes(x = reorder(.data[[v]], n), y = n, fill = .data[[v]])) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
    coord_flip() +
    scale_fill_viridis_d(option = "cividis") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = paste("Frequency:", v),
      x = v, y = "Count"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  print(p)
}

# --- 5b. Observations per Year -----------------------------------------------
if (!is.null(year_col)) {
  p_year_count <- df %>%
    count(.data[[year_col]]) %>%
    ggplot(aes(x = factor(.data[[year_col]]), y = n, fill = factor(.data[[year_col]]))) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.4, size = 4, fontface = "bold") +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Number of Observations per Year", x = "Year", y = "Count") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 13))
  print(p_year_count)
}

# --- 5c. Mean yield by Distance to tree strip (binned) ----------------------
dist_col <- if ("Distance_to_tree_strip" %in% names(df)) "Distance_to_tree_strip" else
  if ("DISTANCE_TO_TREE_STRIP" %in% names(df)) "DISTANCE_TO_TREE_STRIP" else NULL

if (!is.null(dist_col)) {
  dist_yield <- df %>%
    mutate(Dist_bin = cut(.data[[dist_col]],
                          breaks = c(-Inf, 0, 2, 5, 10, 15, Inf),
                          labels = c("0", "1-2", "3-5", "6-10", "11-15", "16+"))) %>%
    select(Dist_bin, all_of(intersect(yield_vars, names(df)))) %>%
    pivot_longer(-Dist_bin, names_to = "Variable", values_to = "Value") %>%
    group_by(Dist_bin, Variable) %>%
    summarise(Mean = mean(Value, na.rm = TRUE), SE = sd(Value, na.rm = TRUE) / sqrt(n()), .groups = "drop")
  
  p_dist <- ggplot(dist_yield, aes(x = Dist_bin, y = Mean, fill = Dist_bin)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
    facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
    scale_fill_viridis_d(option = "rocket", direction = -1) +
    labs(
      title    = "Mean Yield Variables by Distance to Tree Strip",
      subtitle = "Error bars = ±1 SE",
      x        = "Distance to tree strip (m)", y = "Mean Value"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text      = element_text(face = "bold"),
      plot.title      = element_text(face = "bold", size = 13)
    )
  print(p_dist)
}


# =============================================================================
# SECTION 6: CORRELATION ANALYSIS
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 6: CORRELATION ANALYSIS\n")
cat(strrep("=", 70), "\n\n")

# --- 6a. Correlation matrix --------------------------------------------------
cor_data <- df %>%
  select(all_of(numeric_vars)) %>%
  select(where(~ sum(!is.na(.)) >= 5))   # only vars with enough data

cor_mat <- cor(cor_data, use = "pairwise.complete.obs")
cat("Correlation matrix:\n")
print(round(cor_mat, 3))

# --- 6b. Correlation heatmap -------------------------------------------------
p_cor <- ggcorrplot::ggcorrplot(
  cor_mat,
  method   = "square",
  type     = "lower",
  lab      = TRUE,
  lab_size = 2.5,
  colors   = c("#2166ac", "white", "#d6604d"),
  title    = "Correlation Matrix — Pairwise Complete Observations",
  ggtheme  = theme_minimal(base_size = 10)
) +
  theme(plot.title = element_text(face = "bold", size = 13))

print(p_cor)

# --- 6c. Scatter matrix for key variables ------------------------------------
key_scatter <- intersect(
  c("OR_DM", "OR_DM_CONTENT", "WW_DM", "WW_DM_CONTENT",
    "Wheat_DM", "Straw_DM", "Distance_to_tree_strip", "DISTANCE_TO_TREE_STRIP",
    "Litter_DM", "LITTER_DM"),
  names(df)
)

if (length(key_scatter) >= 3) {
  GGally::ggpairs(
    df %>% select(all_of(key_scatter)),
    title = "Scatter Matrix — Key Variables",
    upper = list(continuous = GGally::wrap("cor", size = 3)),
    lower = list(continuous = GGally::wrap("points", alpha = 0.4, size = 1.5)),
    diag  = list(continuous = GGally::wrap("densityDiag", alpha = 0.6))
  ) +
    theme_minimal(base_size = 9) +
    theme(plot.title = element_text(face = "bold", size = 13))
}


# =============================================================================
# SECTION 7: DISTANCE-TO-TREE STRIP EFFECT (KEY RESEARCH VARIABLE)
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 7: DISTANCE-TO-TREE STRIP EFFECT\n")
cat(strrep("=", 70), "\n\n")

if (!is.null(dist_col)) {
  # Scatter + smoother for yield vs distance
  for (yv in intersect(yield_vars, names(df))) {
    p <- ggplot(df, aes(x = .data[[dist_col]], y = .data[[yv]])) +
      geom_jitter(aes(color = if (!is.null(year_col)) factor(.data[[year_col]]) else "All"),
                  width = 0.2, alpha = 0.7, size = 2.5) +
      geom_smooth(method = "loess", se = TRUE, color = "#2c3e50", linewidth = 1) +
      scale_color_brewer(palette = "Set1", name = if (!is.null(year_col)) "Year" else NULL) +
      labs(
        title    = paste(yv, "vs. Distance to Tree Strip"),
        subtitle = "LOESS smoother with 95% CI",
        x        = "Distance to tree strip (m)",
        y        = yv
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"))
    print(p)
  }
}


# =============================================================================
# SECTION 8: LITTER & DECOMPOSITION
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 8: LITTER & DECOMPOSITION\n")
cat(strrep("=", 70), "\n\n")

litter_col <- if ("Litter_DM" %in% names(df)) "Litter_DM" else "LITTER_DM"
decomp_col <- if ("DECOMPOSITION" %in% names(df)) "DECOMPOSITION" else NULL

if (litter_col %in% names(df)) {
  p_litter_dist <- ggplot(df %>% filter(!is.na(.data[[litter_col]])),
                          aes(x = .data[[dist_col]], y = .data[[litter_col]])) +
    geom_point(color = "#6b4226", alpha = 0.7, size = 3) +
    geom_smooth(method = "loess", se = TRUE, color = "#a0522d") +
    labs(
      title = "Litter DM vs. Distance to Tree Strip",
      x     = "Distance to tree strip (m)", y = "Litter DM"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  print(p_litter_dist)
}

if (!is.null(decomp_col) && decomp_col %in% names(df)) {
  p_decomp <- ggplot(df %>% filter(!is.na(.data[[decomp_col]])),
                     aes(x = .data[[decomp_col]])) +
    geom_histogram(bins = 15, fill = "#8e5e3c", color = "white", alpha = 0.85) +
    labs(
      title = "Distribution of Decomposition Rates",
      x     = "Decomposition Rate", y = "Count"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  print(p_decomp)
}


# =============================================================================
# SECTION 9: YEAR × ORIENTATION INTERACTION OVERVIEW
# =============================================================================

if (!is.null(year_col) && !is.null(ori_col) && length(yield_vars) >= 1) {
  cat("\n", strrep("=", 70), "\n")
  cat("  SECTION 9: YEAR × ORIENTATION INTERACTION\n")
  cat(strrep("=", 70), "\n\n")
  
  df_interaction <- df %>%
    select(all_of(c(year_col, ori_col, yield_vars))) %>%
    rename(Year = all_of(year_col), Orientation = all_of(ori_col)) %>%
    mutate(Year = factor(Year)) %>%
    pivot_longer(all_of(yield_vars), names_to = "Variable", values_to = "Value") %>%
    group_by(Year, Orientation, Variable) %>%
    summarise(Mean = mean(Value, na.rm = TRUE), .groups = "drop")
  
  p_interact <- ggplot(df_interaction,
                       aes(x = Year, y = Mean, color = Orientation, group = Orientation)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title    = "Year × Orientation Interaction for Yield Variables",
      subtitle = "Mean values per group",
      x = "Year", y = "Mean Value"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text      = element_text(face = "bold"),
      plot.title      = element_text(face = "bold", size = 13)
    )
  print(p_interact)
}


# =============================================================================
# SECTION 10: DATA QUALITY SUMMARY
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 10: DATA QUALITY SUMMARY\n")
cat(strrep("=", 70), "\n\n")

quality_report <- df %>%
  summarise(across(everything(), list(
    n_obs      = ~ sum(!is.na(.)),
    n_miss     = ~ sum(is.na(.)),
    pct_miss   = ~ round(mean(is.na(.)) * 100, 1),
    n_unique   = ~ n_distinct(., na.rm = TRUE),
    n_outliers = ~ if (is.numeric(.)) {
      q1 <- quantile(., 0.25, na.rm = TRUE)
      q3 <- quantile(., 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      sum(. < q1 - 1.5 * iqr | . > q3 + 1.5 * iqr, na.rm = TRUE)
    } else NA_integer_
  ), .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(),
               names_to  = c("Variable", "Metric"),
               names_sep = "__") %>%
  pivot_wider(names_from = Metric, values_from = value)

cat("Full data quality table:\n")
print(quality_report, n = Inf)


# =============================================================================
# END OF EDA
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("  EDA COMPLETE — Check plots panel for all visualisations\n")
cat(strrep("=", 70), "\n\n")
