
AF_swf <- read_csv('01_Data/AnalysisData/AF_swf.csv')
soil   <- read_csv('01_Data/AnalysisData/SoilGrids/soiltexture.csv')
df  <- left_join(AF_swf, soil, by = 'field')

# fertilization
fert_raw <- gsub("[\u2013\u2212]", "-", df$fertilization_rate_kg_n_p_k_ha_1_year_1)
df$fert_N <- as.numeric(sub("^([0-9.]+).*", "\\1", fert_raw))
df$fert_N[fert_raw == "no fertilization"] <- 0

# assig the af start and the af age from the literature
df <- df |>
  mutate(year_AFplanting = case_when(
    field == "Dornburg"      ~ 2007,
    field == "Forst"         ~ 2010,
    field == "Gladbacherhof" ~ 2020,
    field == "IhingerHof"    ~ 2008,
    field == "Mariensee"     ~ 2008,
    field == "Reiffenhausen" ~ 2011,
    field == "Vechta"        ~ 2019,
    field == "Wendhausen"    ~ 2008
  )) |> 
  mutate(AFage = year - year_AFplanting)

dffull_0909 <- df |> unique()

dffull_0909 <- dffull_0909 |> 
  mutate(ID_nodist = gsub("_\\d+(?:\\.\\d+)?m_", "_", id))

write_csv(dffull_0909, file.path(paste0("01_Data/dffinal_", format(Sys.Date(), "%Y%m%d"), ".csv")))
