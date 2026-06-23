
# Clean teh Environment
rm(list=ls())


df <-  read.csv("data/AnalysisData/20260618_df.csv")

names(df)

# Check coordinates ----------------------------------------
df[is.na(df$lat), ] # clean, 0 rows


# Year Distribution -------------------------------------------------------

hist(df$year)

sink("data/AnalysisData/dataset_names.txt")

cat("=========================================================\n")
cat("ANALYSIS DATASET NAMES - Generated:", format(Sys.time()), "\n")
cat("=========================================================\n\n")

names(df)


sink()
