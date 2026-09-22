library(tidyverse)
library(patchwork)

# --- assuming your data frames are named e.g.:
# df_yield  : columns → field, distance, yield, crop
# df_swf    : columns → field, distance_swf, n_swf, year

df <- mod_data %>%
  mutate(swf_year = case_when(
    year == 2016            ~ 2015,
    year %in% 2017:2019    ~ 2018,
    year %in% 2020:2023            ~ 2021
  ))

table(df$swf_year)
df |> filter(field == "Wendhausen") |> select(year, swf_year) |> unique()

df <- df |> group_by(field, year, crop_unified) |>
  mutate(yield_rel = yield_tha / mean(yield_tha))

fields <- unique(df$field)   # e.g. c("field1", "field2", ...)


annulus_all  <- read_csv(here::here("01_Data/AnalysisData/SWF/swf_annulus_by_distance.csv"))
interior_all  <- read_csv(here::here("01_Data/AnalysisData/SWF/swf_within_field.csv"))
fix_id <- function(x) gsub("Ihinger$", "IhingerHof", x)
annulus_all$id  <- fix_id(annulus_all$id)

for (f in fields) {
  
  # --- a) yield ~ distance, colour = crop -----------------------------------
  p_a <- df |>
    filter(field == f) |>
    ggplot(
      aes(
        x = distance_to_tree_strip,
        y = yield_rel,
        colour = as.factor(swf_year)
      )
    ) +
    geom_smooth(se = TRUE, linewidth = 1.0) +
    scale_colour_brewer(palette = "YlOrRd") +
    labs(
      title  = paste0(f, "   a) Yield over distance"),
      x      = "Distance from tree row (m)",
      y      = "Yield",
      colour = "CORINE Year"
    ) +
    theme_classic(base_size = 16) +
    theme(
      # Axis titles
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      
      # Axis tick labels
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      
      # Legend
      legend.title = element_text(size = 16),
      legend.text  = element_text(size = 15),
      
      # Title
      plot.title = element_text(size = 18),
      
      # Legend at bottom
      legend.position = "none"
    )
  
  
  # --- b) n_swf ~ distance_swf, colour = year -------------------------------
  
  p_b <- annulus_all |>
    filter(id == f) |>
    pivot_longer(
      cols = matches("^(prop_swf|n_swf)\\."),
      names_to = c(".value", "year"),
      names_sep = "\\."
    ) |>
    mutate(year = as.integer(year)) |>
    mutate(year = factor(year)) |>
    unique() |>
    ggplot(
      aes(
        x = distance,
        y = prop_swf,
        colour = year
      )
    ) +
    geom_smooth(se = FALSE, linewidth = 1.0) +
    scale_colour_brewer(palette = "YlOrRd") +
    labs(
      title  = "b) SWF beyond the field",
      x      = "Distance from field border (m)",
      y      = "Proportion SWF",
      colour = "CORINE Year"
    ) +
    theme_classic(base_size = 16) +
    theme(
      # Axis titles
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      
      # Axis tick labels
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      
      # Legend
      legend.title = element_text(size = 16),
      legend.text  = element_text(size = 15),
      
      # Title
      plot.title = element_text(size = 18),
      
      legend.position = "right"
    )
  
  # --- combine side by side (left | right) -----------------------------------
  combined <- p_a | p_b
  
  print(combined)
  
  # optional: save each field as its own file
  ggsave(paste0("03_plots/plot_", f, ".png"), combined, width = 12, height = 5, dpi = 300)
  print("Saved")
}



# shannon versus yield ----------------------------------------------------

mod_data |> 
  distinct(field, year, yield_rel, l_shdi) |> 
  ggplot(aes(y = yield_rel, x = l_shdi, color = as.factor(field), shape = as.factor(year)))+
  geom_point(size = 3)+
  scale_shape_manual(values = c(
    "2016" = 15,
    "2017" = 16,
    "2018" = 17,
    "2019" = 18,
    "2020" = 19,
    "2021" = 10,
    "2022" = 11,
    "2023" = 12
  ))+
  theme_bw()+
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))+
  labs(
    title  = "Shannon VS Relative Yield",
    x      = "Shannons Index in 3000m buffer",
    y      = "Relative Yield",
    colour = "Field",
    shape = "Year"
  )


shape_values <- c(15:19, 20:14)

years <- sort(unique(mod_data$year))

shape_map <- setNames(
  shape_values[seq_along(years)],
  years
)

mod_data |> 
  distinct(field, year, yield_rel, swf_d100) |> 
  ggplot(aes(y = yield_rel, x = swf_d100, color = as.factor(field), shape = as.factor(year) ))+
  geom_point(size = 3) +
  scale_shape_manual(values = c(
      "2016" = 15,
      "2017" = 16,
      "2018" = 17,
      "2019" = 18,
      "2020" = 19,
      "2021" = 10,
      "2022" = 11,
      "2023" = 12
    ))+
  theme_bw()+
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))+
  labs(
    title  = "SWF in 1000m VS Relative Yield",
    x      = "Proportion of SWF in 1000m Buffer",
    y      = "Relative Yield",
    colour = "Field",
    shape = "Year"
  )
    



# Hypothesis Visualisaiton ------------------------------------------------

library(ggplot2)

distance <- c(1, 4, 7, 14, 24)

low_swf <- c(0.65, 0.82, 0.90, 0.90, 0.85)
high_swf <- c(0.65, 0.90, 1.00, 1.00, 0.75)
normal <- c(0.65, 0.90, 1.00, 1.00, 1.00)

low_se <- c(0.04, 0.04, 0.03, 0.03, 0.04)
high_se <- c(0.04, 0.04, 0.03, 0.04, 0.05)
normal_se <- c(0.04, 0.03, 0.03, 0.03, 0.03)

hypothesis_data <- data.frame(
  distance = rep(distance, 3),
  relative_yield = c(low_swf, high_swf, normal),
  se = c(low_se, high_se, normal_se),
  SWF = rep(
    c("Low SWF", "High SWF", "Baseline"),
    each = length(distance)
  )
) |>
  mutate(
    lower = relative_yield - se,
    upper = relative_yield + se
  )

ggplot(
  hypothesis_data,
  aes(
    x = distance,
    y = relative_yield,
    linetype = SWF,
    fill = SWF
  )
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    colour = NA
  ) +
  geom_smooth(
    aes(group = SWF),
    method = "loess",
    se = FALSE,
    linewidth = 1.2
  ) +
  geom_point(size = 3) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = distance,
    limits = c(1, 24)
  ) +
  scale_y_continuous(
    limits = c(0.55, 1.05),
    breaks = seq(0.6, 1.0, 0.1)
  ) +
  scale_linetype_manual(
    values = c(
      "Low SWF" = "dashed",
      "High SWF" = "dotted",
      "Baseline" = "solid"
    )
  ) +
  labs(
    x = "Distance from tree strip (m)",
    y = "Relative yield",
    linetype = NULL,
    fill = NULL
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15)
  )

