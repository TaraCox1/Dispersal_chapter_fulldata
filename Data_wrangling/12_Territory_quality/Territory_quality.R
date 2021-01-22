#Retrieve territory quality measures for natal and natal territories
#During both LastFPID natal and DisperseFPID


rm(list = ls())

getwd()

setwd("Data_wrangling")

library(tibble)
library(dplyr)
library(tidyverse)
library(magrittr)



# Read in data ------------------------------------------------------------

#Data produced by R script 'Identifying_dispersers_and_philos_final_FPID_sub'
disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")


#TQ data
tq <- read.csv("DB_tables_queries/TQEllieSara.csv")





# Merge - for DisperseFPID -----------------------------------------------
#Add natal and disperse territory quality for disperse FPID

#First, disperse territory
disp_philo2 <- disp_philo %>%
                select(BirdID, LastFPIDNatal, SecondLastFPIDNatal, NatalTerritory, DisperseFPID, DisperseTerritoryID)

tq_DisperseFP <- tq %>%
                  rename(DisperseTerritoryID = TerritoryID, 
                    DisperseFPID = FieldPeriodID,
                    TQDisperseTerr_DisperseFP = TQ)

DisperseTerr_tq_DisperseFP <- merge(disp_philo2, tq_DisperseFP, by =c('DisperseTerritoryID', 'DisperseFPID'), all.x = TRUE)  


#Second, add natal territory
tq_DisperseFP %<>% rename(TQNatalTerr_DisperseFP = TQDisperseTerr_DisperseFP,
                          NatalTerritory = DisperseTerritoryID)

NatalTerr_tq_DisperseFP <- merge(DisperseTerr_tq_DisperseFP, tq_DisperseFP, by=c('NatalTerritory', 'DisperseFPID'), all.x = TRUE)





# Merge - for LastFPIDNatal -----------------------------------------------
#Add natal and disperse territory quality for LastFPIDNatal

#First, natal territory
tq_NatalFP <- tq_DisperseFP %>% 
                rename(LastFPIDNatal = DisperseFPID,
                       TQNatalTerr_LastFPIDNatal = TQNatalTerr_DisperseFP)


NatalTerr_tq_NatalFP <- merge(NatalTerr_tq_DisperseFP, tq_NatalFP, by=c('LastFPIDNatal', 'NatalTerritory'), all.x = TRUE)


#Second, add disperse territory  
tq_NatalFP %<>% rename(DisperseTerritoryID = NatalTerritory,
                       TQDisperseTerr_LastFPIDNatal = TQNatalTerr_LastFPIDNatal)

DisperseTerr_tq_NatalFP <- merge(NatalTerr_tq_NatalFP, tq_NatalFP, by=c('LastFPIDNatal', 'DisperseTerritoryID'), all.x = TRUE)
  
  
  
  
  
  
  




