#13/01/21


rm(list = ls())

library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)

getwd()
setwd("Data_wrangling")



# Read in data ------------------------------------------------------------


#First need dispersal data 
#Data produced by R script 'Identifying_dispersers_and_philos_final_FPID_sub'
disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")
disp_philo$DispersalDate <- as.Date(disp_philo$DispersalDate, format = "%Y-%m-%d")


#And malaria data
malaria <- read.csv("DB_tables_queries/qry_Malaria.csv")
malaria$OccasionDate <- as.Date(malaria$OccasionDate, '%d/%m/%Y')
malaria$MalariaConsensus <- as.factor(malaria$MalariaConsensus) #0=negative, 1=positive, 2=faint
malaria$Mismatches <- as.factor(malaria$Mismatches) #whether there is variation in malaria scores for a bird, 0=no, -1=yes
malaria$BirdID <- as.factor(malaria$BirdID)
malaria$BloodID <- as.factor(malaria$BloodID)
malaria$MalariaFieldPeriodID <- as.factor(malaria$FieldPeriodID)



# Descriptives ------------------------------------------------------------


#How many birds tested for personality also have malaria scores
birds_EB <- disp_philo %>% select(BirdID, LastFPIDNatal, SecondLastFPIDNatal, DispersalDate)
birds_malaria <- malaria %>% select(BirdID, Ntests) %>% distinct() 
EB_malaria_summary <- merge(birds_EB, birds_malaria, by = c("BirdID"), all.x = TRUE)

sum(is.na(EB_malaria_summary)) #how many birds haven't had a malaria test
table(EB_malaria_summary$Ntests)


#Most birds get malaria at a young age, so it is likely that most of them will have contracted avian malaria once prior to dispersal
#I don't think there's any evidence to suggest long term physiological effects that could impact decision to disperse



#How many birds were tested for malaria during FP prior to dispersal
malaria_records <- malaria %>% select(BirdID, MalariaFieldPeriodID, MalariaConsensus, OccasionDate) %>% distinct() 
EB_malaria_all <- merge(birds_EB, malaria_records, by = c("BirdID"), all.x = TRUE)

EB_malaria_predisp <- EB_malaria_all %>%
  mutate(MalariaPreDispersal = ifelse(MalariaFieldPeriodID == LastFPIDNatal, 1, 0)) %>%
  filter(MalariaPreDispersal == 1) %>%
  select(BirdID) %>% 
  distinct()

EB_malaria_predisp_MULTIPLE <- EB_malaria_all %>%
  mutate(MalariaPreDispersal = ifelse(MalariaFieldPeriodID == LastFPIDNatal, 1, 0)) %>%
  filter(MalariaPreDispersal == 1) %>%
  mutate(DiffTime = DispersalDate - OccasionDate)#calculate the time between being tested and dispersal date

#Above shows that #days between dispersal and malaria test dates range from 1-487 dats apart 
#This is because a bird can disappear between last FPID natal and first seen in new natal (e.g. floating), meaning there is a 
#large amount of time between last FPID natal and disperse date (as disperse date is midpoint)
#Might be better checking for malaria tests within 2-3 weeks either side of dispersal date 

freq <- count(EB_malaria_predisp_MULTIPLE, 'BirdID')  




#How many birds dispersed within two weeks (either side) of dispersal 

#Calculate time difference between DisperseDate and Occasion date, keep if difference is between 14 and -14s days

test <- EB_malaria_all %>% 
        filter(!(is.na(DispersalDate))) %>%
        mutate(DiffTime = DispersalDate - OccasionDate) %>%
        filter(DiffTime %in% -14:14)


test2 <- EB_malaria_all %>% 
  filter(!(is.na(DispersalDate))) %>%
  mutate(DiffTime = DispersalDate - OccasionDate) %>%
  filter(DiffTime %in% -21:21)


#Few individuals within 2-3 weeks either side of dispersal date
#Likely because dispersal date is an estimated mid-point between last FPID natal and first FPID dispersal territories, when no blood samples are taken

