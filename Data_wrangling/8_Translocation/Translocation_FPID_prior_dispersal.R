##Check whether last FPID natal is during or before a translocation event


rm(list = ls())

getwd()
setwd("Data_wrangling")

library(tidyverse)
library(magrittr)



# Read in data and add translocation column -------------------------------


#Data produced by R script 'Identifying_dispersers_and_philos_final_FPID_sub'
disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")


dispersal_tloc <- disp_philo %>% 
  select(BirdID, Disperse, LastFPIDNatal, LastFPIDNatalPeriodStart, LastFPIDNatalPeriodEnd) %>%
  distinct()







# Determine if dispersal is close to translocation event ------------------
#Add column to enter translocation data
  dispersal_tloc$LastFPIDNatal_Translocation <- NA




#Translocation FPIDs: 120, 45, 33, 119 (FPId prior: 8, 10/12, 32/34, 110, respectively)
#If column equals a transloc field period, add 1
#Note that for some individuals, if field period prior to dispersal was a transloc field period, then I used the field period prior to it
#This is because island censuses and surveys were not conducted in all transloc field periods, meaning group size/pop den/insect values would not be available
#I did this for FPID 110. Therefore, use alternative closest field period to it (112)

#If column value = 110/112, 32/34, , then fill column with 1, if NA then fill column with 0

dispersal_tloc$LastFPIDNatal_Translocation[is.na(dispersal_tloc$LastFPIDNatal_Translocation)] <- 0
dispersal_tloc$LastFPIDNatal_Translocation[dispersal_tloc$LastFPIDNatal==110] <- 1
dispersal_tloc$LastFPIDNatal_Translocation[dispersal_tloc$LastFPIDNatal==112] <- 1
dispersal_tloc$LastFPIDNatal_Translocation[dispersal_tloc$LastFPIDNatal==34] <- 1
dispersal_tloc$LastFPIDNatal_Translocation[dispersal_tloc$LastFPIDNatal==32] <- 1
dispersal_tloc$LastFPIDNatal_Translocation[dispersal_tloc$LastFPIDNatal==10] <- 1
dispersal_tloc$LastFPIDNatal_Translocation[dispersal_tloc$LastFPIDNatal==12] <- 1
dispersal_tloc$LastFPIDNatal_Translocation[dispersal_tloc$LastFPIDNatal==8] <- 1



#Subset to include essential data
dispersal_trans <- dispersal_tloc %>% select(BirdID, LastFPIDNatal_Translocation)


#Save
write.csv(dispersal_trans, '8_Translocation/Translocation.FPID.prior.csv', row.names = FALSE)


