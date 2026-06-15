# publication:
# Highlighting the potential of multilevel statistical models for 
# analysis of individual agroforestry systems

 
  # Load (or install) the required packages 
library(tidyverse) # used to summarize the raw data and for plotting
library(nlme) # used for mixed effects modeling
library(lattice) # used for plotting
library(ggmap) # used to show sampling locations in a graphical form
# ggmap works in wgs84 projection (convert coords if needed)
library(maptools) # used for mapping of the sampling locations
library(sp) # used for mapping of the sampling locations
library(rgdal) # used for mapping of the sampling locations
library(gstat) # used for variogram modelling 
library(outliers) # identification of outliers (tests)
library(EnvStats) # identification of outliers (tests, more detailed)
library(stargazer) # tabularizing the results for reporting
library(partR2) # used for estimation of R^2 values, not recommended here; 
# issues with MerMod objects
# library(mapr) # used for interactive mapping; now retired
library(terra) # used for interactive mapping
library(leaflet) # used for interactive mapping
library(rgdal) # used for interactive mapping

# Step 1: Selection and classification of variables --------------------------
# (a) Identify the research question
# (b) Identify the variables:
#        - Dependent variable
#        - Pre-defined explanatory variable(s)
# (c) Consider relevant spatial/temporal scales for the dependent variable
# (d) Identify the random effect (the grouping variable for 
#     multiple observations that are correlated)
# (e) Visualize your data (see Step 2) to identify additional 
# variables of interest

# Step 2: Data exploration and linear regression fit (with fixed eff.)-----

# Import and check the data
yield <- read.csv("data/Gladbacherhof21/yield.csv") # set the correct path for your 
# directory and make sure that the shapefile for the tree strips is in the 
# correct folder and directory 

head(yield)
names(yield)

# Ensure the variables of interest are coded as factors
sampleID<-as.factor(yield$sample)
trans<-as.factor(yield$transect)
row<-as.factor(yield$row)
direction<-as.factor(yield$direction)
dist<-as.factor(yield$distance)
steepness<-as.factor(yield$slope)
steepness.deg<-as.numeric(yield$slope.deg)
wheat<-as.numeric(yield$grain.t.ha) # grain.t.ha
land.prod<-as.factor(yield$land.prod.basic)
NDVI<-as.numeric(yield$ndvi)
SM<-as.numeric(yield$sm)
x<-as.numeric(yield$xcoord)
y<-as.numeric(yield$ycoord)

# Coerce into a data frame or the models won't run
my.yield=data.frame(wheat,dist,direction,row, trans, sampleID, land.prod, SM,
                    NDVI, steepness.deg, x, y) 

my.yield # use to run models lm, gls and lme

my.yield %>% group_by(dist) %>%         # summary statistics for distance
  summarize(min = min(wheat), 
            max = max(wheat),
            mean = mean(wheat),
            median = median(wheat),
            sd = sd(wheat),
            n = length(wheat))

my.yield %>% group_by(direction) %>%     # summary statistics for direction
  summarize(min = min(wheat), 
            max = max(wheat),
            mean = mean(wheat),
            median = median(wheat),
            sd = sd(wheat),
            n = length(wheat))

# Understand your data, inspect with plots, boxplots, check for outliers
boxplot(wheat~row, data = my.yield) # the variation might be a good indicator 
# that this variable might be fitted as random effect
boxplot(wheat~dist, data = my.yield)
boxplot(wheat~direction, data = my.yield)
plot(wheat~steepness.deg, data = my.yield)
boxplot(wheat~steepness, data = my.yield)
boxplot(wheat~land.prod, data = my.yield)
plot(wheat~SM, data = my.yield)
plot(wheat~NDVI, data = my.yield)

xyplot(wheat ~ dist | row, data = my.yield)
xyplot(wheat ~ row, groups = direction, data = my.yield)

# Looking for outliers (you can use PMCMRplus & ggstatsplot for 
# excellent graphics but check if the 'insight' package is updated or 
# ggstatsplot might not run as of August 2022)

boxplot(my.yield$wheat) # no visible outliers

boxplot(my.yield$wheat ~ my.yield$row) # no visible outliers

boxplot(my.yield$wheat ~ my.yield$direction) # no visible outliers

# suspicious value in row 4

outliers <- boxplot(my.yield$wheat ~ my.yield$row, plot=FALSE)$out
outliers # no outliers 

outlier.test <- tapply(my.yield$wheat, my.yield$row, rosnerTest, k=2)

outlier.test # no statistically sig. outliers

# Develop an lm model
M.lm<-lm(wheat~dist*direction, data=my.yield)
M.lm
summary(M.lm)$coef
drop1(M.lm, test='F') # check if the interaction is significant

# Extract the residuals
E.GLM <- resid(M.lm)

# Inspect your residuals 
# Are there any patterns, if so, violation of assumptions

par(mfrow = c(2, 2))
plot(M.lm)
par(mfrow = c(1, 1))

# There are patterns in the residuals (observed values - fitted values)
# as we move from left to right, the prediction errors increase

# Are the residuals normally distributed? 
# If not, transform to perform (e.g., log)

qqnorm(E.GLM)
qqline(E.GLM, col = "dark green", lwd = 2)
shapiro.test(E.GLM) # normal distribution

# Is there equality of variance?

plot(dist, E.GLM, xlab = "Distance", ylab = "Residuals")
plot(direction, E.GLM, xlab = "Direction", ylab = "Residuals")

# the spread appears to differ for 'distance'
# potential violation of the homogeneity assumption 

# Looking at residual independence in some more detail
# Spatial dependence in transect sampling is a feature of the design

E <- rstandard(M.lm)
spatial.errors <- data.frame(E, 
                             yield$xcoord,
                             yield$ycoord)

spatial.errors # use the headings from the data frame spatial.errors
# for the coordinates function

coordinates(spatial.errors) <- c("yield.xcoord","yield.ycoord")

B1 <- bubble(spatial.errors,"E",col=c("dark green","orange"),
             main="Residuals", xlab="X-coordinates",
             ylab="Y-coordinates") # clustering present

B1 # the bubble plot code from Zuur et al. (2009),
# in addition to displaying the spatial distribution of the residuals,
# it is very well-suited for displaying results from transects
# e.g., by showing at what end of the transects are the errors higher

# The residuals are clustered i.e., they are not spatially independent

# Plot variogram 

vario1 <- variogram(E~1, spatial.errors)

plot(vario1) # variability as a function of distance,
# spatial dependence is evident (there is an increase and leveling off of the 
# data points)

# Plot variogram (multi-directional)

vario2 <- variogram(E~1,
                    spatial.errors,
                    alpha=c(0,45,90,135))

plot(vario2) # spatial dependence in the same direction (isotropy)
# one of the conditions to be met 

# In summary (Part 1):
# normality - check 
# equality of variance - check 
# independence of residual error terms - check 
# be ware of high leverage points (see Cook's distance plot below)

# Outliers - revisited

cooksdist <- cooks.distance(M.lm)
nrow(my.yield)

plot(cooksdist, pch="*", cex=1, 
     main="High leverage points")
abline(h = 4/144, col="dark green") # following the 4/total sample size rule 
# based on Bollen and Jackman (1990)

text(x=1:length(cooksdist)+1, y=cooksdist, 
     labels=ifelse(cooksdist>4/144, names(cooksdist),""), col="dark green")

high.leverage <- as.numeric(names(cooksdist)[(cooksdist > (4/144))])

wheat.out <- my.yield[-high.leverage,] # taking out 8 values 
count(wheat.out)

wheat.out

wheat.out %>% group_by(row) %>% count(dist)

# The high leverage values are found at a distance of 4.5m; 
# instead of removing (multiple) outliers, considering how would the 
# removal of these values change the parameter estimates is essential for 
# reaching the final conclusions

# In summary (Part 2):
# Violation of assumptions; different modelling approach is needed

# Fit linear regression with GLS
# Fitting with GLS is needed to compare lm with lme; the output is the same

f <- formula(wheat~dist*direction, data=my.yield) 

M.gls <- gls(f, method = "ML") # refit lm with gls to allow for
# incorporation of correlation structures and comparisons between models

# Step 3: Fitting a marginal model------------------------------------------
# Investigate a Marginal Model
# (GLS that accounts for residual dependencies)
# benefit: no issues with REML vs. ML bias
# benefit: allows for the variances in the covariance structure to be negative
# which is also possible in lme but needs to be specified (addressed below)

# Visual representation of the actual crop yield distribution

yield.on.map <- ggplot(data = my.yield,
                       mapping = aes(x = x, y = y, color = wheat)) +
  geom_point(size = 3) +
  scale_color_gradientn(colors = c("red", "yellow", "dark green"))

yield.on.map # the samples were collected in a transect (see Figure S1B)
# with 3 samples to the west (left on the map) and the east (right on the map)
# there is a need to account for residual dependencies due to 
# pseudoreplication (multiple plots were harvested across the same site)

# Fit a marginal model to investigate how the introduction of the 
# residual correlation structure affects the quality of the model

?corStruct

MM <- gls(f, method="ML", 
          correlation = corExp(form=~x+y),
          data=my.yield)

MM.Gau <- gls(f, method="ML", 
              correlation = corGaus(form=~x+y),
              data=my.yield)

MM.Lin <- gls(f, method="ML", 
              correlation = corLin(form=~x+y),
              data=my.yield)

MM.Rat <- gls(f, method="ML", 
              correlation = corRatio(form=~x+y),
              data=my.yield)

MM.Sph <- gls(f, method="ML", 
              correlation = corSpher(form=~x+y),
              data=my.yield)

AIC(M.gls, MM, MM.Gau, MM.Lin, MM.Rat, MM.Sph) # including the exponential 
# correlation structure decreases AIC by 35 (for reference, a decrease of 2
# points to a better model)

plot(Variogram(MM, form=~x+y)) 

# include nugget

MM.nugget <- update(MM, correlation = corExp(form=~x+y, nugget=T)) 

anova(MM, MM.nugget) # not significantly different, MM is superior

summary(MM)

anova(MM) # anova is sequential, mind the order in which the explanatory 
# variables are arranged

# Investigating patterns in the residuals 

plot(Variogram(MM, form=~x+y, resType = "n")) # no patterns i.e., 
# the residuals should be have an intercept of ~ 1 and lie in a 
# horizontal line

# Model diagnostics (run the functions at the end of the script first)
# Refit the model with REML prior to running model diagnostics

diagnostics.simple(MM)  # the spread of residuals increases with distance
bubble.plotting(MM)     # the bubble plot shows less clustering than in the 
# M.lm

# The diagnostic plot in line 322 shows that the spread of residuals increases 
# with distance and to a lesser degree with direction.
# This is an issue that can be corrected for by e.g.,:

# Allowing the spread to differ by 'distance' and 'direction'

MM.1 <-gls(f, method="ML", 
           correlation = corExp(form=~x+y),
           weights =     varComb(varIdent(form = ~ 1 | dist), 
                                 varIdent(form = ~ 1 | direction)),
           data =        my.yield)

# Allowing the spread to differ by 'direction'

MM.2 <-gls(f, method="ML", 
           correlation = corExp(form=~x+y),
           weights =     varIdent(form=~1|direction),
           data =        my.yield)

# Allowing the spread to differ by 'distance'

MM.3 <-gls(f, method="ML", 
           correlation = corExp(form=~x+y),
           weights =     varIdent(form = ~ 1 | dist),
           data =        my.yield)

AIC(MM, MM.1, MM.2, MM.3) # MM.1 has the lowest AIC 

# The varIdent does not pertain to multilevel models only, hence it was
# not covered in the Short Communication. For more information, refer to
# e.g., Chapter 4 in Zuur et al. (2009)

# Please run the two functions at the end of the script before running the
# two functions below

bubble.plotting(MM.1) # lower errors
diagnostics.simple(MM.1) # residuals continue to display heterogeneity,
# due to the hierarchical structure of the data, fitting an alternative model
# e.g., with random effects is recommended

# Keep in mind:
# GLS weaknesses: effect size for GLS is usually weaker, 
# especially, when transects (nesting) is concerned 

# Step 4: Fitting LME: Selecting an error structure-----------------------------

# Random intercept-only model 
# [fitted with 'ML' to allow for using log likelihood ratios i.e., anova]

# Looking for the 'grouping' variables i.e., the error structure
# based on the boxplot in line 97, there was some evidence that row might be 
# a suitable grouping variable 

M0.lme=lme(wheat~1,random = ~1|row, 
           method = "ML", data=my.yield)

summary(M0.lme) # Different fixed structure to other models; 
# don't compare with other LME models (or GLS)

# Further investigation of the data as an interactive map indicates that plots
# within a transect display strong spatial patterns i.e., plots closer together 
# (within the same transect) are more similar 

tree.strips = readOGR(dsn = "~/Downloads/Tree strip", layer = "GH1-TreeStrip-3m", 
                      verbose = FALSE)
# load the shapefile in (ensure the path to the folder with shapefile is 
# specified correctly)
tree.strips_xy = spTransform(tree.strips, CRS("+proj=longlat +datum=WGS84"))
pal = colorNumeric(palette = "YlGnBu", domain = yield$grain.t.ha)

wheat.on.map = leaflet(yield) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data = tree.strips_xy,
              fillColor = 'green',
              fillOpacity = 0.5, 
              stroke = FALSE)  %>%
  addCircleMarkers(
    color = ~pal(grain.t.ha),
    stroke = FALSE, 
    radius = 5,
    fillOpacity = 0.5,
    lng = ~lon, lat = ~lat,
    popup = ~as.character(grain.t.ha)) %>%
  addLegend(position = 'bottomright', 
            pal = pal,
            values = ~ grain.t.ha,
            title = 'Wheat yield (t per ha)')

wheat.on.map # tree strips & markers (yellow-green-blue) should be visible
# if not, the file path might not have been set correctly in line 389

# Thus, 'transect' can also be fitted as a random effect

# By employing a random intercept-only model with nested random effects,
# it is possible to e.g., quantify the amount of variance explained by 
# different random effects which can lead to model simplification

M1.lme=lme(wheat~1,random = ~1|row/trans, 
           method = "ML", 
           data=my.yield)
summary(M1.lme)

anova(M0.lme, M1.lme) #  p-value < 0.5; the complex model has lower AIC,
# M1 performs better than M0

# Calculating variance components
SD <- c(0.1880902, 0.3959564) # for M1.lme fitted with REML, not ML; 
# taking SD of the intercept for row and transect

variance <- SD ^ 2

100 * variance / sum(variance) 

# Transect accounts for 82% of variance in the response variable; 
# row accounts for further 18%

# Individual plots are not used as a random effect in this example
# to demonstrate the variance partitioning because there is one
# yield measurement per plot

# Step 5: Fitting LME: Random slope and intercept model-----------------------

# We will consider terrain steepness instead of 'direction'
# and fit a random slope model to demonstrate how the R syntax looks like for
# random slope and intercept models

M.slope.lme=lme(wheat ~ dist * steepness.deg, 
                random = ~ 1 + steepness.deg|row/trans,
                method = "ML",
                data = my.yield)

# No convergence - likely a sample size issue
# (random slope models increase the sample size requirement), 
# it's possible to increase the number of model iterations with e.g.
# lmeControl(niterEM = 5000, msMaxIter = 5000) for the model to run.
# However, this model would have to go through rigorous testing, the same as 
# for the other GLS and LME models. This will not be explored any further
# because data exploration
plot(wheat ~ steepness.deg, col=factor(row)) 
# does not support including terrain steepness as a significant predictor
# i.e. there is limited evidence for linear or nonlinear relationships
# between variables.

# Step 6: Fitting LME: Random intercept model------------------------------
# Fixed effects are included together with the best performing error structure

f

M3.lme=lme(f,random = ~1|row/trans, 
           method = "ML", data=my.yield)

summary(M3.lme) # t-values higher than 2 and -2 for all predictors,
# highlighting their significance 
anova(M3.lme)

diagnostics.simple(M3.lme) # some uneven spread

# Allowing the spread to differ by 'distance'

M3.lme.VI=lme(f, random = ~1|row/trans, 
              weights = varIdent(form = ~ 1 | dist),
              method = "ML", data=my.yield)

summary(M3.lme.VI)
anova(M3.lme, M3.lme.VI) # M3.lme.VI is significantly better

# Any further changes to the varIdent result in observations that are impossible 
# under the null model (that's why it's important to always use the 'summary'
# function and look for any oddities). 

# Step 7: Fitting LME: Error structure parameterization ---------------------------------

# Fit the covariance structure and compare against the parameterized GLS model
# lme has default assumptions that
# 1. the covariance structure for random effects is exchangeable and
# 2. the correlation structure for residuals is independent
# These assumptions can lead to incorrect results

# Model with a specified (corExp) covariance structure
M3.Exp.lme=lme(f,random = ~1|row/trans, 
               corr=corExp(form=~x+y|row/trans),
               method = "ML", data=my.yield)

summary(M3.Exp.lme)

# include other spatial correlation structures 

M3.Gaus.lme=lme(f,random = ~1|row/trans, 
                corr=corGaus(form=~x+y|row/trans),
                method = "ML", data=my.yield)
M3.Lin.lme=lme(f,random = ~1|row/trans, 
               corr=corLin(form=~x+y|row/trans),
               method = "ML", data=my.yield)
M3.Ratio.lme=lme(f,random = ~1|row/trans, 
                 corr=corRatio(form=~x+y|row/trans),
                 method = "ML", data=my.yield) # refit with REML
M3.Spher.lme=lme(f,random = ~1|row/trans, 
                 corr=corSpher(form=~x+y|row/trans),
                 method = "ML", data=my.yield)

AIC(M3.lme, M3.Exp.lme, M3.Gaus.lme,
    M3.Lin.lme,M3.Ratio.lme,M3.Spher.lme) # M3.Exp.lme has the lowest AIC

# In 'nlme' package, the model for the residual
# restricts spatial covariance to points within the same transect,  
# whereas points in different transects are assumed uncorrelated

diagnostics.simple(M3.Exp.lme) # residual spread increases with distance

# Allowing for different variances per 'distance'

M3.Exp.lme.VI=lme(f,  random = ~1|row/trans, 
                  corr =    corExp(form=~x+y|row/trans),
                  weights = varIdent(form = ~ 1 | dist),
                  method = "ML", 
                  data = my.yield)

anova(M3.Exp.lme, M3.Exp.lme.VI) # M3.Exp.lme.VI is significantly better

# Step 8: Model model diagnostics and comparison----------

# Do we have homogeneity of variance and independence of residuals?

# Extract (standardized) residuals
# Inspect the residuals 
# Are there any patterns (heteroscedasticity) , if so, improvements needed
# Residuals per predictor should be evenly distributed & with mean around 0

# Diagnostics (refit with REML before running model diagnostics)

# Original model

M.GLS <- update(M.gls, method = "REML")

# 3 best performing models

LME.1 <- update(M3.lme.VI, method = "REML") # LME with no correlation structure
LME.2 <- update(M3.Exp.lme.VI, method = "REML") # LME with exponential corr. structure
MM.4  <- update(MM.1, method = "REML") # a marginal model 

diagnostics.simple(M.GLS) # known violation of assumptions
bubble.plotting(M.GLS) # clustered errors

diagnostics.simple(LME.1) # good residual spread
bubble.plotting(LME.1) # less clustered errors

diagnostics.simple(LME.2) # some patterns in the residuals but
# good spread per predictor
bubble.plotting(LME.2) # less clustered errors

diagnostics.simple(MM.4) # residuals vs. fitted values are acceptable but 
# the residual spread per distance is problematic
bubble.plotting(MM.4) # less clustered errors

# Residual plots should be included in e.g., Appendices or the Results section

# Tip for reporting

stargazer(LME.1, LME.2, MM.4, type='text', digits=2)

# The estimates are very similar. In our case, we reject MM.3 based on diagnostic
# plots in lieu of LME.2 which has a lower AIC value than LME.1 and fairly  
# acceptable diagnostic plots. The LME.2 is a model with a parameterized 
# covariance structure.

# Further checks 
# Fitting actual vs predicted wheat yields for LME.1, LME.2 and MM.4

par(mfrow=c(1,1))

F.LME.1 = fitted(LME.1)
plot(F.LME.1, my.yield$wheat,
     xlab = 'Fitted',
     ylab = 'Response',
     xlim = c(1, 6),
     ylim = c(1, 6))
abline(0,1) # ideally, the fit should be tighter 

plot(LME.1, wheat~fitted(.)|row) 

F.LME.2 = fitted(LME.2)
plot(F.LME.2, my.yield$wheat,
     xlab = 'Fitted',
     ylab = 'Response',
     xlim = c(1, 6),
     ylim = c(1, 6))
abline(0,1) # similarly, the fit should be tighter 

plot(LME.2, wheat~fitted(.)|row)

F.MM.2 = fitted(MM.4)
plot(F.MM.2, my.yield$wheat,
     xlab = 'Fitted',
     ylab = 'Response',
     xlim = c(1, 6),
     ylim = c(1, 6)) 
abline(0,1) # a suspected missing covariate

plot(MM.4, wheat~fitted(.)|row) # refitting and including a new covariate
# (e.g., SM) and trialing e.g.,  Zuur's 10 step backward
# model selection procedure would be recommended

# In order to improve the model and further reduce the bubble plot clustering,
# we could, for example: 
#                       1. Add covariates through backward or forward selection
#                          (GH1 has variable topography and site conditions)
#                       2. Add interaction terms (if needed for new covariates),
#                       3. Investigate non-linear relationships between variables
#                          (mind we only have 3 categorical levels for distance
#                          and 2 categorical levels for direction)

# Step 9: Avoiding hasty conclusions with visualizations  -------------

# Distance from tree strips is a significant predictor (removing 'dist' makes
# the model weaker). However, concluding that it is the effect of the trees as  
# opposed to the locations of the sampling plots which drives the patterns
# observed in the data might be a hasty conclusion. 

lattice::xyplot(wheat~dist | row, groups=row, 
                data=my.yield, type=c('p','r'), auto.key=F)

# Wheat yield is highest at 4.5m - this is odd, if the trees were negatively
# affecting the crop yield (only 1 year after planting),
# the yield should be highest in the middle of the field 

lattice::xyplot(wheat~dist | direction, groups=row, 
                data=my.yield, type=c('p','r'), auto.key=T)

# There is some evidence for the yield increasing further away from tree strips
# but not for both directions. However, the high yield at 4.5 remains a 
# consistent pattern.

# In conclusion, we fitted some extremely complex (and very site-specific)  
# models but based them on a research question that does not necessarily fit our
# observations. Land management (i.e., a change in the width of machinery  
# to accommodate tree strips has resulted in more seeds being delivered to
# the sampling locations at 4.5m distance from the tree strips)
# is a more likely driver of the observed trends than the impact of trees
# in this very young agroforestry system. This highlights the importance of 
# preliminary data collection and has implications for
# future sampling schemes (to improve internal validity).

# We hope this example has served as a good introduction to using
# a subset of multilevel models in agroforestry research. 

# This script highlights the main aspects of data exploration, analysis and
# diagnostics which might be helpful to researchers who are new to
# multilevel models. The data set was provided to encourage readers of this
# piece to modify the script (e.g., re-run the models with two levels
# of the distance variable), ask alternative research questions, test models
# with other variance-covariance structures (e.g., only transect), non-linear
# relationships (for additional covariates), etc., and to think about potential 
# pitfalls of analyzing agroforestry data. 

# We are looking forward to seeing how the data analysis methods in  
# agroforestry research evolve in the upcoming years.

# Speeding up the workflow

bubble.plotting<-function(mltlvl.model) {
  resid.mltlvl<-resid(mltlvl.model, type="normalized")
  spatial.errors<-data.frame(resid.mltlvl, 
                             yield$xcoord,
                             yield$ycoord)
  coordinates(spatial.errors)<-c("yield.xcoord","yield.ycoord")
  plot.errors<- bubble(spatial.errors,"resid.mltlvl",
                       col=c("dark green","orange"),
                       main="Residuals", xlab="X-coordinates",
                       ylab="Y-coordinates") 
  return(plot.errors)
}

diagnostics.simple<-function(mltlvl.model) {
  mltlvl.errors  <- resid(mltlvl.model, type="normalized")
  mltlvl.fitted  <- fitted(mltlvl.model)
  op<-par(mfrow=c(2,2), mar=c(5,5,3,2), cex.lab = 1.5, cex.axis = 1)
  MyYlab="Residuals"
  plot(x=mltlvl.fitted,y=mltlvl.errors,xlab="Fitted values", ylab=MyYlab)
  boxplot(mltlvl.errors ~ dist, data=my.yield, 
          xlab="Distance from tree rows", 
          ylab=MyYlab)
  boxplot(mltlvl.errors ~ direction, data=my.yield,
          xlab="Direction",
          ylab=MyYlab)
  qqnorm(mltlvl.errors)
  qqline(mltlvl.errors, col = "dark green", lwd = 2)
  shapiro.test(mltlvl.errors)
}
