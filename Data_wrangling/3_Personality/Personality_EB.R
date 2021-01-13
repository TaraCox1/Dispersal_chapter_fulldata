#Subset dispersers to only individuals that have personality scores prior to dispersal

rm(list = ls())
library(dplyr)

getwd()

setwd("Data_wrangling")



# Read in data ------------------------------------------------------------

disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")

str(disp_philo)

disperse <- disp_philo[(disp_philo$Disperse==1),]

philopatric <- disp_philo[(disp_philo$Disperse==0),]



# Add earliest personality score date to both dfs -------------------------


#Read in data produced from the query p_Personality_collated_dates
alldata <- read.csv('Personality_all.csv')


#Novel environment data only
environmentonly <- subset(alldata, select = c("BirdID", "EnvironmentScore", "OccasionDate"))
CLEANenvironment <- na.omit(environmentonly)

CLEANenvironment$OccasionDate <- as.Date(CLEANenvironment$OccasionDate , "%d/%m/%Y")


#Take the earliest personality score for these individuals
envir_scores <- CLEANenvironment %>% 
  group_by(BirdID) %>% 
  slice(which.min(OccasionDate))


#Change to compatible format for dates
envir_scores$OccasionDate <- as.Date(envir_scores$OccasionDate , "%d/%m/%Y")


#Novel object data only
objectonly <- subset(alldata, select = c("BirdID", "ObjectScore", "OccasionDate"))
CLEANobject <- na.omit(objectonly)

CLEANobject$OccasionDate <- as.Date(CLEANobject$OccasionDate , "%d/%m/%Y")

obj_scores <- CLEANobject %>% 
  group_by(BirdID) %>% 
  slice(which.min(OccasionDate))

obj_scores$OccasionDate <- as.Date(obj_scores$OccasionDate , "%d/%m/%Y")



#Merge list of dispersers or philopatrics with the list of their earliest object and exploration scores
#Merged dfs will have less individuals. This is because not every type of personality data exists
#for every individual that has a personality score. E.g. have an envir score, but not an obj score,
#some have been tested for activity/stress, but not exploration.
dispersers_envir_date <- merge(envir_scores, disperse, by='BirdID')
dispersers_obj_date <- merge(obj_scores, disperse, by='BirdID')

philos_envir_date <- merge(envir_scores, philopatric, by='BirdID')
philos_obj_date <- merge(obj_scores, philopatric, by='BirdID')










# Identify individuals tested for personality after dispersal --------


#If OccasionDate>PeriodStart then 0, but 1 if PeriodStart>OccasionDate
#I.e. if the personality date (OccasionDate) is later than the field season in which an individual dispersed,
#(PeriodStart)then put '0' to indicate we cannot include this individual. 
#Vice versa for PeriodStart>OccasionDate, with '1' indicating we can include the individual in the df.
#If OccasionDate=Period Start, then also put 1
dispersers_envir_date$Prior_dispersal <- ifelse(dispersers_envir_date$OccasionDate>dispersers_envir_date$DispersalDate,0,
                                                ifelse(dispersers_envir_date$OccasionDate<dispersers_envir_date$DispersalDate,1,
                                                       ifelse(dispersers_envir_date$OccasionDate==dispersers_envir_date$DispersalDate,1,NA)))

dispersers_obj_date$Prior_dispersal <- ifelse(dispersers_obj_date$OccasionDate>dispersers_obj_date$DispersalDate,0,
                                              ifelse(dispersers_obj_date$OccasionDate<dispersers_obj_date$DispersalDate,1,
                                                     ifelse(dispersers_obj_date$OccasionDate==dispersers_obj_date$DispersalDate,1,NA)))


#The below script has been commented out - it was taken from the earlier scripts that excluded individuals that were not tested for personality prior to dispersal

#Only keep individuals with personality dates prior to dispersal
#dispersers_envir_date_2 <- dispersers_envir_date[!(dispersers_envir_date$Prior_dispersal=="0"),]
#dispersers_obj_date_2 <- dispersers_obj_date[!(dispersers_obj_date$Prior_dispersal=="0"),]


#Remove Prior_dispersal column
#dispersers_envir_date_3<- subset(dispersers_envir_date_2, select = -c(Prior_dispersal))
#dispersers_obj_date_3 <- subset(dispersers_obj_date_2, select = -c(Prior_dispersal))
#philos_envir_date_2 <- subset(philos_envir_date, select = -c(Prior_dispersal))
#philos_obj_date_2 <- subset(philos_obj_date, select = -c(Prior_dispersal))








# Create and save dfs -----------------------------------------------
#Rearrange dfs columns to assist with rbind



envir_final <- rbind(dispersers_envir_date, philos_envir_date)
obj_final <- rbind(dispersers_obj_date, philos_obj_date)

write.csv(envir_final,'Envir_score_pre_disp.csv', row.names = FALSE)
write.csv(obj_final,'Obj_score_pre_disp.csv', row.names = FALSE)


















