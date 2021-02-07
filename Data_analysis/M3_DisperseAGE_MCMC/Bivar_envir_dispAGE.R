m2 <- MCMCglmm(cbind(envir.score, orderNormAge2) ~ trait-1 +
                   trait:Sex +
                   at.level(trait,1):Age +
                   at.level(trait,1):Age2 +
                   at.level(trait,1):Colour +
                   at.level(trait,1):Branch +
                   at.level(trait,1):Novel.environment.assay.number, 
                 #     at.level(trait,2):scale(InsectAbundance),
                 random =~ at.level(trait,1):ID,
                 rcov =~ us(trait):units,
                 family = c("poisson","gaussian"),
                 prior = hd.haggis.simp,
                 nitt=1500000,
                 burnin=100000,
                 thin=350,
                 verbose = TRUE,
                 pr=TRUE,
                 data = as.data.frame(dispenv6))

#Check if response data are normal
unique_age1_env <- dispenv %>% select(BirdID, Age_at_disp1) %>% distinct() #first get rid of duplicate rows 
bestNormalize(unique_age1_env$Age_at_disp1) #use best normalize on unique bird distances 
age1_env <- unique_age1_env$Age_at_disp1 #apply recommended best normalize method - orderNorm (ORQ)
orderNorm_age1_envir <- bestNormalize::orderNorm(age1_env)
unique_age1_env$orderNormAge1 <- predict(orderNorm_age1_envir)

#Check distribution of transformed distance
skewness(unique_age1_env$orderNormAge1, na.rm = TRUE) #not skewed
shapiro.test(unique_age1_env$orderNormAge1) #>0.05 is normal distribution
hist(unique_age1_env$orderNormAge1)



#Repeat for age_age_disp2
unique_age2_env <- dispenv %>% select(BirdID, Age_at_disp2) %>% distinct() #first get rid of duplicate rows 
bestNormalize(unique_age2_env$Age_at_disp2) #use best normalize on unique bird distances 
age2_env <- unique_age2_env$Age_at_disp2 #apply recommended best normalize method - orderNorm (ORQ)
orderNorm_age2_envir <- bestNormalize::orderNorm(age2_env)
unique_age2_env$orderNormAge2 <- predict(orderNorm_age2_envir)

#Check distribution of transformed distance
skewness(unique_age2_env$orderNormAge2, na.rm = TRUE) #not skewed
shapiro.test(unique_age2_env$orderNormAge2) #>0.05 is normal distribution
hist(unique_age2_env$orderNormAge2)



#Add to main df 
unique_age_env <- merge(unique_age1_env, unique_age2_env, by = c('BirdID'))
dispenv6 <- merge(unique_age_env, dispenv5, by = c('BirdID', 'Age_at_disp1', 'Age_at_disp2'), all.y = TRUE)
