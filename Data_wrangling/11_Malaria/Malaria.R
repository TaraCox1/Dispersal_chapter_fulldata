#13/01/21


rm(list = ls())

library(dplyr)
library(plyr)
library(ggplot2)

getwd()
setwd("Data_wrangling")



# Read in data ------------------------------------------------------------


#First need dispersal data 
#Data produced by R script 'Identifying_dispersers_and_philos_final_FPID_sub'
disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")


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
birds_EB <- disp_philo %>% select(BirdID, LastFPIDNatal, SecondLastFPIDNatal)
birds_malaria <- malaria %>% select(BirdID, Ntests) %>% distinct() 
EB_malaria_summary <- merge(birds_EB, birds_malaria, by = c("BirdID"), all.x = TRUE)

sum(is.na(EB_malaria_summary)) #how many birds haven't had a malaria test
table(EB_malaria_summary$Ntests)



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
  filter(MalariaPreDispersal == 1) 

freq <- count(EB_malaria_predisp_MULTIPLE, 'BirdID')  

