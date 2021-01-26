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
fullenv <- read.csv("envir_all.csv")


fullenv$ID<-as.factor(fullenv$BirdID)
fullenv$Disperse <- as.factor(fullenv$Disperse)
fullenv$Inherit<-as.factor(fullenv$Inherit)
fullenv$DispersalMonth <- as.factor(fullenv$DispersalMonth)
fullenv$DispersalSeason <- as.factor(fullenv$DispersalMonth2)
fullenv$Age_at_disp1 <- as.numeric(fullenv$Age_at_disp1)
fullenv$Age_at_disp2 <- as.numeric(fullenv$Age_at_disp2)
fullenv$Reached_min_age_1 <- as.factor(fullenv$Reached_min_age_1)
fullenv$Reached_min_age_2 <- as.factor(fullenv$Reached_min_age_2)
fullenv$DispersalYear <- as.integer(fullenv$DispersalYear)
fullenv$BirthYear1 <- as.integer(fullenv$BirthYear1)
fullenv$BirthYear2 <- as.integer(fullenv$BirthYear2)
fullenv$PopDensity.old <- as.integer(fullenv$PopDensity.old)
fullenv$PopDensity.all <- as.integer(fullenv$PopDensity.all)
fullenv$GroupSize.old <- as.integer(fullenv$GroupSize.old)
fullenv$GroupSize.all <- as.integer(fullenv$GroupSize.all)
fullenv$TranslocationYN <- as.factor(fullenv$TranslocationYN)
fullenv$Translocation.fp <- as.factor(fullenv$Translocation.FPID.prior)
fullenv$Distance <- as.numeric(fullenv$Distance)
fullenv$NatalTerritorySize <- as.numeric(fullenv$NatalTerritorySize)
fullenv$DisperseTerritorySize <- as.numeric(fullenv$DisperseTerritorySize)
fullenv$InsectAbundance <- as.numeric(fullenv$InsectAbundance)
fullenv$InsectDensity <- as.numeric(fullenv$InsectDensity)
fullenv$Sex <- as.factor(fullenv$SexEstimate)
fullenv$SexEstimate <- ifelse(fullenv$SexEstimate == 1, "M", "F")


fullenv$envir.score<-as.integer(fullenv$Novel.environment.score)
fullenv$Person<-as.factor(fullenv$Observer)
fullenv$Status<-as.factor(fullenv$Social.status)#Sub dom
fullenv$Age<-as.numeric(fullenv$Age)
fullenv$Age2<-as.numeric(fullenv$Age2)
fullenv$Novel.environment.assay.number<-as.integer(fullenv$Testno)
fullenv$NovelEnvirASSAY_REP<-as.integer(fullenv$Testno)
fullenv$Mass<-as.numeric(fullenv$StndBody.mass)
fullenv$Interval<-as.integer(fullenv$Time.between.tests)
fullenv$Colour<-as.factor(fullenv$TentColour)
fullenv$Branch<-as.factor(fullenv$ExplorationBranchOrientation)
fullenv$Treerelease<-as.factor(fullenv$ReleaseMethod)
fullenv$TentPoles<-as.factor(fullenv$TentPoles)
fullenv$BranchHeight<-as.factor(fullenv$BranchHeight)
fullenv$Date <- as.Date(fullenv$Date, "%d/%m/%Y")
fullenv$Season2<-as.numeric(fullenv$Season)






# 2. Tidy data ------------------------------------------------------------

#Subset to just dispersing birds
dispenv <- fullenv %>% filter(Disperse == 1)



#####################Rescale/transform data###################

#Check if response data are normal
unique_distance <- dispenv %>% select(BirdID, Distance) %>% distinct() #first get rid of duplicate rows 
bestNormalize(unique_distance$Distance) #use best normalize on unique bird distances 
distance <- unique_distance$Distance #apply recommended best normalize method - orderNorm (ORQ)
orderNorm_envir <- bestNormalize::orderNorm(distance)
unique_distance$orderNormDistance <- predict(orderNorm_envir)

#Check distribution of transformed distance
skewness(unique_distance$orderNormDistance, na.rm = TRUE) #not skewed
shapiro.test(unique_distance$orderNormDistance) #>0.05 is normal distribution
hist(unique_distance$orderNormDistance)



#Standardize any continuous fixed effects
predictors <- dispenv %>% select('BirdID', 'InsectDensity', 'PopDensity.old', 
                                       'PopDensity.all', 'GroupSize.old', 
                                       'GroupSize.all', 'InsectAbundance') #subset continuous predictors, keep birdID so I can subset to unique rows below

predictors <- predictors %>% 
              distinct() %>% #subset to unique rows 
              select(!(BirdID)) %>% #remove birdid as unique rows have been identified 
              rename(InsectDensityScaled = InsectDensity,
                     InsectAbundanceScaled = InsectAbundance,
                     PopDensity.oldScaled = PopDensity.old,
                     PopDensity.allScaled = PopDensity.all,
                     GroupSize.oldScaled = GroupSize.old,
                     GroupSize.allScaled = GroupSize.all)
                        
s.predictors <- scale(predictors) #scale predictors


#cbind transformed distance and fixed effects
transformed_dis_fe <- cbind(unique_distance, s.predictors)

#Merge with main df
dispenv2 <- merge(dispenv, transformed_dis_fe, by =c('BirdID', 'Distance'))


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

corvif(s.predictors) #strong correlation between insect abundance + density, G/S old + all, PopDen old + all, which is ok because they won't both be included in models. 





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

m1.1.data <- dispenv2 %>%
             filter(!is.na(InsectAbundance))




# Model 1.1: including all fixed effects for dispersal and envir scores --------

#NOTE: Cannot scale() envir score because Poisson distribution cannot have negative integers 
m1.1 <- MCMCglmm(cbind(envir.score, orderNormDistance) ~ trait-1 +
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
                      at.level(trait,1):Novel.environment.assay.number +
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
                    data = as.data.frame(m1.1.data))


#Runs, but produces warning messages, need a more informative prior.
#'some fixed effects are not estimable and have been removed. Use singular.ok=TRUE to sample these effects, but use an informative prior!#

summary(m1.1)




# Model 1.2: including all fixed effects for dispersal and envir scores, ALTERNATIVE  ------------------------------------------------------------

#Alternative group size, pop density and translocation methods of calculation
#Uses all individuals in a population (young and old) for group size + pop density, as well as translocation FP prior to dispersal
m1.2 <- MCMCglmm(cbind(envir.score, orderNormDistance) ~ trait-1 +
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
                   at.level(trait,1):Novel.environment.assay.number +
                   at.level(trait,1):Testno +
                   at.level(trait,2):PopDensity.allScaled +
                   at.level(trait,2):GroupSize.allScaled +
                   at.level(trait,2):TranslocationYN + 
                   at.level(trait,2):InsectAbundanceScaled,
                 random =~ us(trait):ID + us(trait):Person,
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

m1.3 <- MCMCglmm(cbind(envir.score, orderNormDistance) ~ trait-1 +
                      trait:Sex +
                      at.level(trait,1):Age +
                      at.level(trait,1):Age2 +
                      at.level(trait,1):Colour +
                      at.level(trait,1):Branch +
                      at.level(trait,1):Novel.environment.assay.number, 
                 #     at.level(trait,2):scale(InsectAbundance),
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




# Check model 1.2 outputs
plot(m1.4$VCV)


#Remember: 
#deviance information criterion (DIC) is similar to AIC
#G-structure shows random effects (co)variances i.e. among individual (co)variances 
#R-structure shows residual (co)variances i.e. within individual (co)variances
#Location effects shows fixed effects results

#Each section provides the mean of the posterior distribution returned by MCMCglmm, upper and lower bounds of 95%CI, effective sample size and pMCMC (for fixed effects only)
#traitDistance:traitDistance.units should have an effective sample size of 0 and a low estimate because of how we defined the parameter 
summary(m1.4)

#Check convergence 
#Passed? (should all pass to be good), there are lots of fails
heidel.diag(m1.1.hd.2$Sol)
heidel.diag(m1.1.hd.2$VCV)


#<2 z-score, so non-significant is good (tests similarity between first and last 10% of iterations)
t <- geweke.diag(m1.1.hd.2$Sol)
geweke.diag(m1.1.hd.2$VCV)


# Plot model outputs 

#Check repeatability of envir exploration
mcmc_prop_E <- m1.4$VCV[,"traitenvir.score:traitenvir.score.ID"]/(
  m1.4$VCV[,"traitenvir.score:traitenvir.score.ID"] +
   # m1.1.hd.2$VCV[,"traitenvir.score:traitenvir.score.Person"] +
    m1.4$VCV[,"traitenvir.score:traitenvir.score.units"]
)


plot(mcmc_prop_E)
mean(mcmc_prop_E)



#Check correlation when two rand effects (person and ID) are in the model
mcmc_E_B_fit_cor_Efit <- m1.1.hd.2$VCV[,"traitboxcoxDistance:traitenvir.score.ID"]/
  (sqrt(m1.1.hd.2$VCV[,"traitboxcoxDistance:traitboxcoxDistance.ID"])+
     sqrt(m1.1.hd.2$VCV[,"traitboxcoxDistance:traitboxcoxDistance.Person"])*
     sqrt(m1.1.hd.2$VCV[,"traitenvir.score:traitenvir.score.ID"])+
     sqrt(m1.1.hd.2$VCV[,"traitenvir.score:traitenvir.score.Person"]))



#Check correlation when one rand effect (ID) is in the model
mcmc_m1.1_fit_cor_Efit <- m1.1$VCV[,"traitorderNormDistance:traitenvir.score.ID"]/
  (sqrt(m1.1$VCV[,"traitorderNormDistance:traitorderNormDistance.ID"])*
     sqrt(m1.1$VCV[,"traitenvir.score:traitenvir.score.ID"]))


#Plot
df_mcmc_cors1 <- tibble(Traits = c("Environment exploration, Distance"),
                       Estimate = c(mean(mcmc_m1.1_fit_cor_Efit)),
                       Lower = c(HPDinterval(mcmc_m1.1_fit_cor_Efit)[,"lower"]),
                       Upper = c(HPDinterval(mcmc_m1.1_fit_cor_Efit)[,"upper"]))




ggplot(df_mcmc_cors1, aes(x = Traits, y = Estimate)) +
  geom_pointrange(aes(ymin = Lower,
                      ymax = Upper)) +
  geom_hline(yintercept = 0,
             linetype = "dotted", alpha = 0.3) +
  scale_x_discrete(limits = c("Environment exploration, Distance")) +
  labs(x = "Trait combinations",
       y = "Correlation (Estimate +/- 95% CIs)") +
  ylim(-3,3) +
  coord_flip() +
  theme_classic()


























































#####################Remove unnecessary repeats in fixed effects data###################

#Remove personality data and subset to distinct rows 
dispenv3 <- dispenv2 %>% 
            select(-(Novel.environment.score | StndBody.mass | Weather | Observer | TentColour |
                       ExplorationBranchOrientation | ReleaseMethod | Time.between.tests | Social.status |
                       TentPoles | BranchHeight | Days.into.season | Testno | Ageclass | Age | Age2 |
                       Age_sq | Season | BodyMass | envir.score | Person | Status | Novel.environment.assay.number |
                       NovelEnvirASSAY_REP | Mass | Interval | Colour | Branch | Treerelease | Season2 | ID | Sex)) %>%
            distinct() %>% 
            group_by(BirdID) %>%
            mutate(AssayID = 1:n()) #group by BirdID and create a row ID 



#Remove dispersal data, group by bird ID and add an ID for assay number 
dispenv4 <- dispenv2 %>%
            select(BirdID | Novel.environment.score | StndBody.mass | Weather | Observer | TentColour |
                  ExplorationBranchOrientation | ReleaseMethod | Time.between.tests | Social.status |
                  TentPoles | BranchHeight | Days.into.season | Testno | Ageclass | Age | Age2 |
                  Age_sq | Season | BodyMass | envir.score | Person | Status | Novel.environment.assay.number |
                  NovelEnvirASSAY_REP | Mass | Interval | Colour | Branch | Treerelease | Season2 | ID | Sex) %>%
            arrange(BirdID, Novel.environment.assay.number) %>%
            group_by(BirdID) %>%
            mutate(AssayID = 1:n())


#Merge together 
dispenv5 <- merge(dispenv3, dispenv4, by =c('BirdID', 'AssayID'), all = TRUE)
