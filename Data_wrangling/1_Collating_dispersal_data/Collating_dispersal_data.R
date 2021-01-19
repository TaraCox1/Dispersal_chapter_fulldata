#This script merges the status and corresponding territory for each FPID across an individuals lifetime - taken from the database with natal territory ID from the pedigree 
#As there are some conflicts between the database and pedigree for juvenile statuses, which were reviewed and amended manually
#The final df contains every status and corresponding territory an individual has resided in


# Read in data ------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(dplyr)

getwd()
setwd("Data_wrangling")


#Read in csv produced by Access query t_TerritoryandStatus'
#Contains statuses and territories for every individual
database_terr <- read.csv('1_Collating_dispersal_data/D_resident_territories_w_dates.csv')


#And pedigree data from database queries 'qry_PedigreeFinal_sig_envir_4' and 'qry_PedigreeFinal_sig_obj_4' 
pedigree_terr_e <- read.csv('./Pedigree/Pedigree1990_2018_envir.csv')
pedigree_terr_e$Status <- "CH"
pedigree_terr_o <- read.csv('./Pedigree/Pedigree1990_2018_obj.csv')
pedigree_terr_o$Status <- "CH"


#Check data
str(database_terr)
str(pedigree_terr_o)
str(pedigree_terr_e)


# Prepare data ------------------------------------------------------------


names(pedigree_terr_e)[2]<-"BirdID"
names(pedigree_terr_e)[6]<-"FieldPeriodID"
names(pedigree_terr_e)[13]<-"TerritoryNumber"
names(pedigree_terr_e)[14]<-"TerritoryID"


#Filter pedigree data to >=0.8 
pedigree_terr_SIG_e <- pedigree_terr_e %>%
  filter(p >=0.8)


#Remove duplicate rows
pedigree_terr_SIG_e <- pedigree_terr_SIG_e %>% distinct()


#Remove data that are not needed, including mumFPID, p, FieldPeriodID, O_TerrNum, O_TerrID, dad_FPID, MumStatus + TerritoryNumber
pedigree_terr_SIG_e <- pedigree_terr_SIG_e[, -c(1, 3:5, 9:13)]
database_terr <- database_terr[, -c(3,4,9)]


#Rearrange columns so both df match
pedigree_terr_SIG_e <- pedigree_terr_SIG_e[,c(1, 5, 6, 2, 3, 4)]





# Repeat for object data --------------------------------------------------
names(pedigree_terr_o)[2]<-"BirdID"
names(pedigree_terr_o)[6]<-"FieldPeriodID"
names(pedigree_terr_o)[13]<-"TerritoryNumber"
names(pedigree_terr_o)[14]<-"TerritoryID"


#Filter pedigree data to >=0.8 
pedigree_terr_SIG_o <- pedigree_terr_o %>%
  filter(p >=0.8)


#Remove duplicate rows
pedigree_terr_SIG_o <- pedigree_terr_SIG_o %>% distinct()


#Remove data that are not needed, including mumFPID, p, FieldPeriodID, O_TerrNum, O_TerrID, dad_FPID, MumStatus + TerritoryNumber
pedigree_terr_SIG_o <- pedigree_terr_SIG_o[, -c(1, 3:5, 9:13)]


#Rearrange columns so both df match
pedigree_terr_SIG_o <- pedigree_terr_SIG_o[,c(1, 5, 6, 2, 3, 4)]



#Merge database and pedigree data
total <- rbind(pedigree_terr_SIG_o, pedigree_terr_SIG_e, database_terr)
total <- total %>% distinct()




# Error check -------------------------------------------------------------
#Pedigree and database observations for juvenile statuses can conflict
#Where the pedigree uses genetics to provide an individual with CH status and belongs to the genetic mother's territory. the database uses field observations 
#Sometimes, either side may be incorrect, meaning each individual's conflict must be reviewed on a case by case basis, then amended manually
#This script identifies conflicts and corrects them manually


#Make a copy of final df
conflict_check <- total


#Subset to rows only containing juveile statuses
check_juv <- subset(conflict_check, Status == "CH" | Status == "FL" | Status == "OFL")


#Subset to BirdID and TerritoryID, as we are interested in knowing whether each individual has the same territory throughout juvenile stages
check_juv2 <- subset(check_juv, select = c('BirdID', 'TerritoryID'))


#Remove duplicate entries 
#If juv resident territories are correct, they should should only be resident at one territory
check_juv3 <- check_juv2 %>% distinct()


#Remove individuals that are resident in one territory, so we can see which individuals have conflicts
check_juv4 <- check_juv3[!table(check_juv3$BirdID)[as.character(check_juv3$BirdID)] <2,]


#Place all conflicts in a separate df to easily review
check_juv5 <- unique(check_juv4$BirdID)


#Once each individual has been reviewed, manually make corrections to the 'total' df
correct_total <- total


#Done with ifstatement, where if the BirdID, Status and/or FPID match my search, replace the territoryID with the correct value
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 6563 & correct_total$Status == 'FL' & correct_total$FieldPeriodID == 144, 24, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 3518 & correct_total$Status == 'OFL' & correct_total$FieldPeriodID == 54, 28, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 6546 & correct_total$Status == 'OFL', 75, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 6548 & correct_total$Status == 'OFL', 71, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 6037 & correct_total$Status == 'FL' & correct_total$FieldPeriodID == 122, 90, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 6141 & correct_total$Status == 'OFL', 9, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 6564 & correct_total$Status == 'OFL', 81, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 5685 & correct_total$Status == 'FL', 14, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 1731 & correct_total$Status == 'CH', 80, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 1877 & correct_total$Status == 'CH', 125, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 5262 & correct_total$Status == 'CH', 66, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 5771 & correct_total$Status == 'CH', 47, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 5787 & correct_total$Status == 'CH', 9, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 5787 & correct_total$Status == 'CH', 9, correct_total$TerritoryID)
#Below is just an incorrect residency territory number caused by territory merges - seee queries and tables document
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 6026 & correct_total$TerritoryID == 85, 82, correct_total$TerritoryID)
correct_total$TerritoryID <- ifelse(correct_total$BirdID == 6693 & correct_total$Status == 'OFL', 481, correct_total$TerritoryID)


#Sanity check
#Does the new df contain all individuals from original 'total' df?
all.equal(correct_total$BirdID, total$BirdID)


#Are there definitely no conflicts? Re-run above code
sanity <- correct_total
sanity <- subset(correct_total, Status == "CH" | Status == "FL" | Status == "OFL")


sanity2 <- subset(sanity, select = c('BirdID', 'TerritoryID'))


sanity3 <- sanity2 %>% distinct()


sanity4 <- sanity3[!table(sanity3$BirdID)[as.character(sanity3$BirdID)] <2,]


sanity5 <- unique(sanity4$BirdID)
sanity5


#If 0, then save
write.csv(correct_total, 'Pedigree_database_terri_merged.csv', row.names = FALSE) 

