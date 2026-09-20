library(tidyverse)
library(patchwork)

# --- assuming your data frames are named e.g.:
# df_yield  : columns → field, distance, yield, crop
# df_swf    : columns → field, distance_swf, n_swf, year

df <- df %>%
  mutate(swf_year = case_when(
    year == 2016            ~ 2015,
    year %in% 2017:2019    ~ 2018,
    year >= 2020            ~ 2021
  ))

table(df$swf_year)
df |> filter(field == "Wendhausen") |> select(year, swf_year) |> unique()

fields <- unique(df$field)   # e.g. c("field1", "field2", ...)


annulus_all  <- read_csv(here::here("01_Data/AnalysisData/SWF/swf_annulus_by_distance.csv"))
interior_all  <- read_csv(here::here("01_Data/AnalysisData/SWF/swf_within_field.csv"))
fix_id <- function(x) gsub("Ihinger$", "IhingerHof", x)
annulus_all$id  <- fix_id(annulus_all$id)

for (f in fields) {
  
  # --- a) yield ~ distance, colour = crop -----------------------------------
  p_a <- df |>
    filter(field == f) |>
    ggplot(aes(x = distance_to_tree_strip, y = yield_log, colour = as.factor(swf_year))) +
    geom_smooth(se = TRUE, linewidth = 0.9) +
    scale_colour_brewer(palette = "YlOrRd") +
    labs(
      title    = paste0(f, "  —  a)  Yield over distance"),
      x        = "Distance from tree row (m)",
      y        = "Yield",
      colour   = "Crop"
    ) +
    theme_classic(base_size = 12) +
    theme(legend.position = "bottom")
  
  # --- b) n_swf ~ distance_swf, colour = year --------------------------------
  p_b <- annulus_all |>
    filter(id == f) |>
    pivot_longer(
      cols      = matches("^(prop_swf|n_swf)\\."),
      names_to  = c(".value", "year"),
      names_sep = "\\.") |>
    mutate(year = as.integer(year)) |> 
    mutate(year = factor(year)) |>
    unique() |> 
    ggplot(aes(x = distance, y = n_swf, colour = year)) +
    geom_smooth(se = FALSE) +
    scale_colour_brewer(palette = "YlOrRd") +
    labs(
      title    = paste0(f, "  —  b)  N_SWF over distance"),
      x        = "Distance from tree row (m)",
      y        = "N_SWF",
      colour   = "Year"
    ) +
    theme_classic(base_size = 12) +
    theme(legend.position = "bottom")
  
  # --- combine side by side (left | right) -----------------------------------
  combined <- p_a | p_b
  
  print(combined)
  
  # optional: save each field as its own file
  #ggsave(paste0("03_plots/plot_", f, ".png"), combined, width = 12, height = 5, dpi = 300)
  print("Saved")
  }
