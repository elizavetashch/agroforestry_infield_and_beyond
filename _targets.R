library(targets)

tar_option_set(
  packages = c(
    "refund", "mgcv", "gratia",
    "dplyr", "tidyr", "readr", "ggplot2"
  )
  # format left at default "rds" — no qs/qs2 dependency needed
)

tar_source("02_scripts/Functions/")           # plain relative path, no here::here()

AF_SWF_FILE <- "01_Data/AF_swf.csv"          # plain relative path

list(
  
  tar_target(af_swf_file,
             AF_SWF_FILE,
             format = "file"),
  
  tar_target(df_raw,
             readr::read_csv(af_swf_file, show_col_types = FALSE)),
  
  tar_target(dflong,
             prepare_dflong(df_raw)),
  
  tar_target(swf_prepared,
             prepare_swf_data(df_raw)),
  
  tar_target(mod_data_base,
             join_mod_data(dflong, swf_prepared)),
  
  tar_target(mod_data,
             add_pca_scores(mod_data_base)),
  
  tar_target(swf_inputs,
             build_swf_inputs(mod_data)),
  
  tar_target(m_pfr_int,
             fit_pfr_interaction(mod_data, swf_inputs)),
  
  tar_target(m_pfr,
             fit_pfr_base(mod_data, swf_inputs)),
  
  tar_target(m_gam_full,
             fit_gam_full(mod_data)),
  
  tar_target(aic_table,
             compare_aic(m_pfr_int, m_pfr, m_gam_full)),
  
  tar_target(diagnostics_pfr_int,
             run_gam_diagnostics(m_pfr_int, label = "m_pfr_int")),
  
  tar_target(term_df,
             compute_term_variance(m_pfr_int)),
  
  tar_target(p_term_variance,
             gg_term_variance(term_df)),
  
  tar_target(p_swf_distance_effect,
             gg_swf_distance_effect(m_pfr_int, mod_data)),
  
  tar_target(p_swf_main_effect,
             gg_swf_main_effect(m_pfr_int, mod_data)),
  
  tar_target(p_beta0_curve,
             gg_beta0_curve(m_pfr_int)),
  
  tar_target(p_beta_surface,
             gg_beta_surface(m_pfr_int))
)