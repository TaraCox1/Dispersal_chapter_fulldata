#Bivariate models for personality and dispersal

rm(list = ls())

getwd()
setwd("Data_analysis/M1_DisperseYN_MCMC")


library(lme4)
library(MCMCglmm)
library(tidyverse)
library(broom)
library(nadiv)
library(bestNormalize)
library(moments)
library(car)


# 1. Read in data ---------------------------------------------------------



#Novel envir data
fullobj <- read.csv("obj_all.csv")


fullobj$ID<-as.factor(fullobj$BirdID)
fullobj$Disperse <- as.factor(fullobj$Disperse)
fullobj$Inherit<-as.factor(fullobj$Inherit)
fullobj$DispersalMonth <- as.factor(fullobj$DispersalMonth)
fullobj$DispersalSeason <- as.factor(fullobj$DispersalMonth2)
fullobj$Age_at_disp1 <- as.numeric(fullobj$Age_at_disp1)
fullobj$Age_at_disp2 <- as.numeric(fullobj$Age_at_disp2)
fullobj$Reached_min_age_1 <- as.factor(fullobj$Reached_min_age_1)
fullobj$Reached_min_age_2 <- as.factor(fullobj$Reached_min_age_2)
fullobj$DispersalYear <- as.integer(fullobj$DispersalYear)
fullobj$BirthYear1 <- as.integer(fullobj$BirthYear1)
fullobj$BirthYear2 <- as.integer(fullobj$BirthYear2)
fullobj$PopDensity.old <- as.integer(fullobj$PopDensity.old)
fullobj$PopDensity.all <- as.integer(fullobj$PopDensity.all)
fullobj$GroupSize.old <- as.integer(fullobj$GroupSize.old)
fullobj$GroupSize.all <- as.integer(fullobj$GroupSize.all)
fullobj$TranslocationYN <- as.factor(fullobj$TranslocationYN)
fullobj$Translocation.fp <- as.factor(fullobj$Translocation.FPID.prior)
fullobj$Distance <- as.numeric(fullobj$Distance)
fullobj$NatalTerritorySize <- as.numeric(fullobj$NatalTerritorySize)
fullobj$DisperseTerritorySize <- as.numeric(fullobj$DisperseTerritorySize)
fullobj$InsectAbundance <- as.numeric(fullobj$InsectAbundance)
fullobj$InsectDensity <- as.numeric(fullobj$InsectDensity)
fullobj$Sex <- as.factor(fullobj$SexEstimate)
fullobj$SexEstimate <- ifelse(fullobj$SexEstimate == 1, "M", "F")


fullobj$obj.score<-as.integer(fullobj$Novel.object.score)
fullobj$Person<-as.factor(fullobj$Observer)
fullobj$Status<-as.factor(fullobj$Social.status)#Sub dom
fullobj$Age<-as.numeric(fullobj$Age_sd)
fullobj$Age2<-as.numeric(fullobj$Age_sq_sd)
fullobj$Novel.object.assay.number<-as.integer(fullobj$Testno)
fullobj$NovelobjASSAY_REP<-as.integer(fullobj$Testno)
fullobj$Mass<-as.numeric(fullobj$StndBody.mass)
fullobj$Interval<-as.integer(fullobj$Time.between.tests)
fullobj$Colour<-as.factor(fullobj$TentColour)
fullobj$Branch<-as.factor(fullobj$ExplorationBranchOrientation)
fullobj$Treerelease<-as.factor(fullobj$ReleaseMethod)
fullobj$TentPoles<-as.factor(fullobj$TentPoles)
fullobj$BranchHeight<-as.factor(fullobj$BranchHeight)
fullobj$Season2<-as.numeric(fullobj$Season)






# 2. Tidy data ---------------------------------------------

#Subset to just dispersing birds
dispobj <- fullobj %>% filter(Disperse == 1)



########Rescale/transform data########

#Check if response data are normal
unique_distance_o <- dispobj %>% select(BirdID, Distance) %>% distinct() #first get rid of duplicate rows 
bestNormalize(unique_distance_o$Distance) #use best normalize on unique bird distances 
distance_o <- unique_distance_o$Distance #apply recommended best normalize method - orderNorm (ORQ)
boxcox_obj <- bestNormalize::boxcox(distance_o)
unique_distance_o$boxcoxDistance <- predict(boxcox_obj)

#Check distribution of transformed distance
skewness(unique_distance_o$boxcoxDistance, na.rm = TRUE) #not skewed
shapiro.test(unique_distance_o$boxcoxDistance) #>0.05 is normal distribution
hist(unique_distance_o$boxcoxDistance)



#Standardize any continuous fixed effects
predictors_o <- dispobj %>% select('BirdID', 'InsectDensity', 'PopDensity.old', 
                                 'PopDensity.all', 'GroupSize.old', 
                                 'GroupSize.all', 'InsectAbundance') #subset continuous predictors, keep birdID so I can subset to unique rows below

predictors_o <- predictors_o %>% 
  distinct() %>% #subset to unique rows 
  select(!(BirdID)) %>% #remove birdid as unique rows have been identified 
  rename(InsectDensityScaled = InsectDensity,
         InsectAbundanceScaled = InsectAbundance,
         PopDensity.oldScaled = PopDensity.old,
         PopDensity.allScaled = PopDensity.all,
         GroupSize.oldScaled = GroupSize.old,
         GroupSize.allScaled = GroupSize.all)

s.predictors_o <- scale(predictors_o) #scale predictors


#cbind transformed distance and fixed effects
transformed_dis_fe_o <- cbind(unique_distance_o, s.predictors_o)

#Merge with main df
dispobj2 <- merge(dispobj, transformed_dis_fe_o, by =c('BirdID', 'Distance'))


#Check vif - first create function
corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  #correlation part
  cat("Correlations of the variables\n\n")
  tmp_cor <- cor(dataz,use="complete.obs")
  print(tmp_cor)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}

corvif(s.predictors_o) #strong correlation between insect abundance + density, G/S old + all, PopDen old + all, which is ok because they won't both be included in models. 








#3. Define prior -------------------------------------------------------------------

#Copy prior from haggis tutorial 
hd.haggis.simp = list(R = list(V = diag(c(1, 0.0001), 2, 2), nu = 1.002, fix=2),
                      G = list(G1 = list(V = diag(2), nu = 2,
                                         alpha.mu = rep(0,2),
                                         alpha.V = diag(25^2,2,2))))




#Merge haggis prior with HD prior (merged with haggis prior as this considers that lack of within individual variance of distance that occurs due to only one measure)
hd.haggis = list(R = list(V = diag(c(1, 0.0001), 2, 2), nu = 1.002, fix=2),
                 G = list(G1 = list(V = diag(2), nu = 2,
                                    alpha.mu = rep(0,2),
                                    alpha.V = diag(c(1000,1000))),
                          G2 = list(V = diag(2), nu = 2,
                                    alpha.mu = rep(0,2),
                                    alpha.V = diag(25^2,2,2))))






# 4. Run bivariate models -------------------------------------------------------------------
#Bivariate model for novel environment exploration and disperse distance

m1.1.odata <- dispobj2 %>%
  filter(!is.na(InsectAbundance))




# Model 1.1: including all fixed effects for dispersal and envir scores --------

#NOTE: Cannot scale() envir score because Poisson distribution cannot have negative integers 
#Does not run with hd.haggis prior - error states to use a stronger prior 
m1.1_o <- MCMCglmm(cbind(obj.score, boxcoxDistance) ~ trait-1 +
                   trait:Sex +
                   at.level(trait,1):Mass +
                   at.level(trait,1):Age +
                   at.level(trait,1):Age2 +
                   at.level(trait,1):Season2 +
                   at.level(trait,1):Colour +
                   at.level(trait,1):Branch +
                   at.level(trait,1):Treerelease +
                   at.level(trait,1):TentPoles +
                   at.level(trait,1):BranchHeight +
                   at.level(trait,1):Status +
                   at.level(trait,1):Interval +
                   at.level(trait,1):Weather +
                   at.level(trait,1):Novel.object.assay.number +
                   at.level(trait,1):Testno +
                   at.level(trait,2):PopDensity.oldScaled +
                   at.level(trait,2):GroupSize.oldScaled +
                   at.level(trait,2):TranslocationYN + 
                   at.level(trait,2):InsectAbundanceScaled,
                 random =~ us(trait):ID,
                 rcov =~ us(trait):units,
                 family = c("poisson","gaussian"),
                 prior = hd.haggis.simp,
                 nitt=750000,
                 burnin=50000,
                 thin=175,
                 verbose = TRUE,
                 pr=TRUE,
                 data = as.data.frame(m1.1.odata))






# Model 1.2: including all fixed effects for dispersal and envir scores, ALTERNATIVE  ------------------------------------------------------------

#Alternative group size, pop density and translocation methods of calculation
#Uses all individuals in a population (young and old) for group size + pop density, as well as translocation FP prior to dispersal
m1.2_o <- MCMCglmm(cbind(obj.score, boxcoxDistance) ~ trait-1 +
                   trait:Sex +
                   at.level(trait,1):Mass +
                   at.level(trait,1):Age +
                   at.level(trait,1):Age2 +
                   at.level(trait,1):Season2 +
                   at.level(trait,1):Colour +
                   at.level(trait,1):Branch +
                   at.level(trait,1):Treerelease +
                   at.level(trait,1):TentPoles +
                   at.level(trait,1):BranchHeight +
                   at.level(trait,1):Status +
                   at.level(trait,1):Interval +
                   at.level(trait,1):Weather +
                   at.level(trait,1):Novel.object.assay.number +
                   at.level(trait,1):Testno +
                   at.level(trait,2):PopDensity.allScaled +
                   at.level(trait,2):GroupSize.allScaled +
                   at.level(trait,2):TranslocationYN + 
                   at.level(trait,2):InsectAbundanceScaled,
                 random =~ us(trait):ID,
                 rcov =~ us(trait):units,
                 family = c("poisson","gaussian"),
                 prior = hd.haggis,
                 nitt=750000,
                 burnin=50000,
                 thin=175,
                 verbose = TRUE,
                 pr=TRUE,
                 data = as.data.frame(m1.1.data))








# Model 1.3: only fixed effects with a significant effect on dispersal distance and envir score -----------------

m1.3_o <- MCMCglmm(cbind(obj.score, boxcoxDistance) ~ trait-1 +
                   trait:Sex +
                   at.level(trait,1):Age +
                   at.level(trait,1):Age2 +
                   at.level(trait,1):Novel.object.assay.number, 
                    at.level(trait,2):scale(InsectAbundance),
                 random =~ us(trait):ID,
                 rcov =~ us(trait):units,
                 family = c("poisson","gaussian"),
                 prior = hd.haggis.simp,
                 nitt=750000,
                 burnin=50000,
                 thin=175,
                 verbose = TRUE,
                 pr=TRUE,
                 data = as.data.frame(dispenv5))


