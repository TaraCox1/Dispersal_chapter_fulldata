#Collate all data needed for dispersal analyses

rm(list = ls())

library(tidyr)
library(dplyr)
library(magrittr)

getwd()
setwd("Data_wrangling")

# Read in data ------------------------------------------------------------


#1. Disperser vs philopatric
disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")
disp_philo_filt <- disp_philo %>% 
                   select(BirdID, Disperse, Inherit, DispersalDate, NatalTerritory)
                          


#2. Age at dispersal
age <- read.csv('4_Age_at_disp/Age_at_dispersal_all.csv')


#3. Population density
pop <- read.csv('5_Pop_density/Pop_density.csv')


#4. Group size
group <- read.csv('6_Group_size/Group_size.csv')


#5.1. Translocation in any FPID spent in natal territory
transloc <- read.csv('8_Translocation/Translocation.csv')
transloc %<>% distinct()


#5.2 Translocation last FPID seen in natal territory
#transloc.last.fp <- read.csv('8_Translocation/Translocation.FPID.prior.csv')


#6. Distance
distance <- read.csv('9_Dispersal_distance/Dispersal_distance.csv')


#7.1. Insect abundance
#Produced by script Insect_4
insect_ab <- read.csv('10_Insect_abundance/Insect_abundance.csv')


#7.2. Insect density
#Produced by script Insect_Lewis
insect_den <- read.csv('10_Insect_abundance/Insect_density.csv')
insect_den %<>% select(BirdID, InsectDensity)


#8. Sex
sex <- read.csv('DB_tables_queries/sys_SexEstimates.csv')
sex <- subset(sex, select = c('BirdID', 'SexEstimate'))


#Personality data
envir <- read.csv('Novel.envir.rep.her.data.csv')
obj <- read.csv('Novel.obj.rep.her.data.csv')

envir %<>%
      select(BirdID, Novel.environment.score, StndBody.mass, BodyMass, Weather, 
             Observer, TentColour, ExplorationBranchOrientation, ReleaseMethod,
             Time.between.tests,  Social.status, TentPoles, BranchHeight, 
             Days.into.season, Testno, Ageclass, Age, Age2, Age_sq)

obj %<>%
  select(BirdID, Novel.object.score, StndBody.mass, BodyMass, Weather, 
         Observer, TentColour, ExplorationBranchOrientation, ReleaseMethod,
         Time.between.tests,  Social.status, TentPoles, BranchHeight, 
         Days.into.season, Testno, Ageclass, Age_sd, Age_sq, Age_sq_sd)


# Merge -------------------------------------------------------------------


#For all dispersers and philopatrics (regardless of if they reached min age at dispersal, or have values for all fixed effects)
all_age <- merge(disp_philo_filt, age, by=c('BirdID'))

all_pop <- merge(all_age, pop, by=c('BirdID'))

all_gs <- merge(all_pop, group, by=c('BirdID'))

all_transloc <- merge(all_gs, transloc, by=c('BirdID'), all.x = TRUE)

all_distance <- merge(all_transloc, distance, by=c('BirdID'), all.x = TRUE)

all_insect_den <- merge(all_distance, insect_den, by=c('BirdID'))

all_sex <- merge(all_insect_den, sex, by=c('BirdID'))





#Create separate df, binding dispersal info with individuals tested for novel envir
envir2 <- merge(all_sex, envir, by=c('BirdID')) 

obj2 <- merge(all_sex, obj, by=c('BirdID')) 







# Save --------------------------------------------------------------------

write.csv(envir_all, '11_Collate/envir_all.csv', row.names = FALSE)
write.csv(obj_all, '11_Collate/obj_all.csv', row.names = FALSE)

write.csv(envir_all, '../envir_all.csv', row.names = FALSE)
write.csv(obj_all, '../obj_all.csv', row.names = FALSE)

