##Check whether dispersal FPID is during or directly following a translocation event
#Note, if FPID last resident in natal territory was 110, then it is calculated as the season before 110 (as no surveys etc. were calculated during this season)



rm(list = ls())

getwd()
setwd("Data_wrangling")

library(plyr)
library(dplyr)
library(ggplot2)




# Read in data and add translocation column -------------------------------


#Data produced by R script 'Identifying_dispersers_and_philos_final_FPID_sub'
disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")


dispersal_tloc <- disp_philo %>% 
  select(BirdID, Disperse, LastFPIDNatal, LastFPIDNatalPeriodStart, LastFPIDNatalPeriodEnd) %>%
  distinct()







# Determine if dispersal is close to translocation event ------------------
#Add column to enter translocation data
dispersal_tloc$Translocation_FPID_prior <- NA




#If column value = 110, 32/34 (34 overlaps with DS transloc), then fill column with 1, if NA then fill column with 0
dispersal_tloc$Translocation_FPID_prior[is.na(dispersal_tloc$Translocation_FPID_prior)] <- 0
dispersal_tloc$Translocation_FPID_prior[dispersal_tloc$LastFPIDNatal==110] <- 1
dispersal_tloc$Translocation_FPID_prior[dispersal_tloc$LastFPIDNatal==32] <- 1
dispersal_tloc$Translocation_FPID_prior[dispersal_tloc$LastFPIDNatal==34] <- 1


#Fill translocation of philoaptrics to NA
#dispersal.trans$Translocation.FPID.prior[is.na(dispersal.trans$FieldPeriodID)] <- NA


#Subset to include essential data
dispersal_trans <- dispersal_trans[, -c(3:5)]


#Save
write.csv(dispersal.trans, 'Data_wrangling/8_Translocation/Translocation.FPID.prior.csv', row.names = FALSE)


#If a philopatric has lived through either 110 or 112, then should I note this in another column?
