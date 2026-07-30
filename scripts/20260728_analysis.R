
# Analysis script 

library(readr)
library(ggplot2)
library(car)
fertilization <- read_csv("data/DataDescription_Fertilization.csv")
treeharvest <- read_csv("data/DataDescription_TreeHarvest.csv")
df <- read_csv("analysis_data/20260705_all_fields.csv")
df <- df[!is.na(df$distance_to_tree_strip), ] # delete control treatments
swf <- read_csv("analysis_data/20260726_propswf.csv")


ggplot(df, aes(x=distance_to_tree_strip, y=yield_tha, color=crop_unified)) + 
  geom_point()+
  geom_smooth()

yield ~ distance + crop 


mod <- glm(yield_tha ~ distance_to_tree_strip + crop , data = df, family = "gaussian")

par(mfrow = c(2,2))
plot(mod)


Anova(mod, type = "II", test.statistic = "F")
summary(mod)
out.anova <- Anova(mod, test.statistic = "F")    
R2.value <- sum(out.anova[,"Sum Sq"][1:nrow(out.anova)-1]) / sum(out.anova[,"Sum Sq"])
R2.value

# mixed effect 

df <- df |> 
  mutate(yield_scaled = scale(yield_tha),
         distance_to_tree_strip_scaled = scale(distance_to_tree_strip))

mod <- lmer(
  yield_scaled ~ distance_to_tree_strip_scaled + crop_unified +
    (1 | field), data = df)


sjPlot::plot_model(mod)
sjPlot:: tab_model(mod)











#################################
# THE EFFECT
# OF SURROUNDING LANDSCAPE
# ON PRODUCTIVITY
#################################
# Author: Dr. Elina Takola
# Date: 29.11.2025
#################################
set.seed(999)
options(scipen = 999) # disable scientific notation
library(lme4)
library(lmerTest)
library(Matrix)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom.mixed)
library(plotly)
library(ggeffects)
library(DHARMa)
library(forcats)
library(mgcv)       
library(patchwork)
library(psych)
library(corrplot)
library(spdep) # Moran's I test


############ 
# Data exploration
############ 
# Let's check collinearity first
num_vars <- df[, sapply(df, is.numeric)]  # select numeric columns
par(mfrow=c(1,1))
num_vars <- df[, sapply(df, is.numeric)]  # select numeric columns
cor_matrix <- cor(num_vars, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", tl.cex = 0.4, tl.col = "black")

# Distribution of the response variable
ggplot(df, aes(x = lrr)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(alpha = 0.2, fill = "blue") +
  labs(title = "Distribution of LRR", x = "LRR", y = "Density")

# LRR per harvest year
ggplot(df, aes(x = harvest.year.by.median, y = lrr)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "LRR over Harvest Year", x = "Harvest Year", y = "LRR")

# Facet by crop_type and treatment (trend line per facet, x = harvest_year)
ggplot(df, aes(x = harvest.year.by.median, y = lrr)) +
  geom_point(alpha = 0.5, aes(color = as.factor(treatment))) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(crop.type.orig.grouped.big ~ treatment,
             labeller = labeller(.rows = label_wrap_gen(width = 15),
                                 .cols = label_wrap_gen(width = 15))) +
  labs(title = "LRR by Crop Type and Treatment over Harvest Year",
       x = "Harvest Year", y = "LRR") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

crop_table <- table(df$crop.type.orig, df$poll.dependent)
colnames(crop_table) <- c("Pollinator_Independent", "Pollinator_Dependent")
write.csv(crop_table, "crop_pollinator_table.csv", row.names = TRUE)





############ 
# Model selection
############ 
df1000 <- df %>% select(-contains("2500"), -contains("5000"), -contains("measurement.id"), -contains("ma.id"), -contains("control.id"))
df2500 <- df %>% select(-contains("1000"), -contains("5000"), -contains("measurement.id"), -contains("ma.id"), -contains("control.id"))
df5000 <- df %>% select(-contains("2500"), -contains("1000"), -contains("measurement.id"), -contains("ma.id"), -contains("control.id"))
response <- "lrr"
rand <- "(1|study.id)"
fixed_drop <- c("lrr", "study.id")
thr <- 0.30
imp_num  <- function(x) { x[is.na(x)] <- median(x, na.rm = TRUE); x }
imp_fact <- function(x) { tab <- table(x, useNA = "no"); if (length(tab)) x[is.na(x)] <- names(which.max(tab)); x }
is_const <- function(x) {
  if (is.numeric(x)) sd(x, na.rm = TRUE) == 0 else length(na.omit(unique(x))) <= 1
}

# 5 km
na_counts <- sapply(df5000, function(x) sum(is.na(x)))
keep1 <- names(df5000)[!(sapply(df5000, function(x) all(is.na(x))) | sapply(df5000, is_const))]
df1 <- df5000[keep1]
na_prop <- sapply(df1, function(x) mean(is.na(x)))
Xcols <- setdiff(names(df1)[na_prop <= thr], fixed_drop)
df2 <- df1
for (nm in Xcols) {
  if (is.numeric(df2[[nm]])) df2[[nm]] <- imp_num(df2[[nm]])
  else if (is.factor(df2[[nm]]) || is.character(df2[[nm]])) df2[[nm]] <- factor(imp_fact(as.factor(df2[[nm]])))
}
form_full <- as.formula(paste0(response, " ~ ", paste(Xcols, collapse = " + "), " + ", rand))
rows_ok <- complete.cases(df2[, c(response, "study.id")])
df_fit <- droplevels(df2[rows_ok, ])
mod_full <- lmer(form_full, data = df_fit, REML = FALSE, na.action = na.omit)
step_res <- step(mod_full, reduce.fixed = TRUE, reduce.random = FALSE, ddf = "Satterthwaite")
best_model5000 <- get_model(step_res)
coef_tab <- as.data.frame(coef(summary(best_model5000)))
coef_tab$term <- rownames(coef(summary(best_model5000)))
rownames(coef_tab) <- NULL
coef_tab <- coef_tab[, c("term", setdiff(names(coef_tab), "term"))]
write.csv(coef_tab, "best_mod_coefficients5000.csv", row.names = FALSE)
capture.output(summary(best_model5000), file = "best_mod_summary5000.txt")
capture.output(step_res, file = "step_selection_log5000.txt")
writeLines(c("Full model formula:", deparse(formula(mod_full)), "",
             "Final selected model formula:", deparse(formula(best_model5000))),
           con = "bestmodel5000.txt")
