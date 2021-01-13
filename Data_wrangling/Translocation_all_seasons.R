#Translocation event has ever occured during individuals lifetime (from age 6 months)



rm(list = ls())
getwd()
setwd("Data_wrangling")

library(lubridate)
library(dplyr)
library(mefa)


# 1. Read in required data ------------------------------------------------

#First need focal individual data 
Envir <- read.csv('Envir_score_pre_disp.csv')
Obj <- read.csv('Obj_score_pre_disp.csv')

Envir <- subset(Envir, select = -c(EnvironmentScore, OccasionDate))
Obj <- subset(Obj, select = -c(ObjectScore, OccasionDate))


personality <- rbind(Envir, Obj)
personality <- personality %>%
  distinct() %>%
  select(BirdID, LastFPIDNatal, LastFPIDNatalPeriodStart, LastFPIDNatalPeriodEnd)

str(personality)


#Next need birth data
birth <- read.csv("DB_tables_queries/tblBirdID.csv")
birth <- subset(birth, select = c(BirdID, BirthDate, BirthFieldPeriodID))
birth$BirthDate <- as.Date(birth$BirthDate, "%d/%m/%Y")


#Merge
personality <- merge(birth, personality, by = c("BirdID"))


#Add 6 months to birth date as this is the earliest age an individual can disperse
personality$sixmonths <- personality$BirthDate %m+% months(6)


#And FPID data
FPID <- read.csv("DB_tables_queries/tblFieldPeriodIDs.csv")
FPID_dates <- subset(FPID, select = c('FieldPeriodID', 'PeriodStart', 'PeriodEnd'))
FPID_dates$PeriodStart <- as.Date(FPID_dates$PeriodStart , "%d/%m/%Y")
FPID_dates$PeriodEnd <- as.Date(FPID_dates$PeriodEnd , "%d/%m/%Y")






# 2. Identify every FPID an individual has lived in their natal territory for since 6 months old --------

BirdID <- personality

#Repeat bird IDs 126 times as there are 126 FPIDs
BirdID <- rep(BirdID, times = 126)

#Repeat FPIDs 354 times as there are 354 birds
FPIDs_reps <- rep(FPID_dates, each = 354)

#Bind together
FPIDs_sixm <- cbind(FPIDs_reps, BirdID)

test <- FPIDs_sixm

#If the FPIDs end date is greater than the date an individual was six months 
#old, as well as the FPID start date less than the last day an individual was 
#seen in their natal territory, put Y, else N. I.e. if the FPID falls within 
#the range of dates an individual was resident in their natal territory, put Y
test$FPIDDuringNatalResidency <- ifelse(test$PeriodEnd>=test$sixmonths & test$LastFPIDNatalPeriodStart>=test$PeriodStart, "Y", "N")

#Subset to FPIDs within the natal residency date range i.e. 'Y'
FPIDs_individual_in_natal <- test[(test$FPIDDuringNatalResidency=="Y"),]



#Check for missing individuals 

#Missing individuals either do not have  FPID between 6month age and date last seen in natal territory because an
#individual is first seen during a field period at <6months old (these FPs excluded as individuals do not disperse before this age)
# then a) disappear or b) are resident at a new territory the following season 
FPIDS_BIRDID <- subset(FPIDs_individual_in_natal, select = c('BirdID', 'BirthDate'))

FPIDS_BIRDID <- FPIDS_BIRDID %>% distinct()

(missing <- anti_join(personality, FPIDS_BIRDID, 'BirdID'))



# 3. Identify individuals resident in natal territory during translocation FPID(s) --------


#Check whether individuals lived through translocation FPIDs 33 or 119
translocs <- FPIDs_individual_in_natal

translocs$Translocation <- ifelse(FPIDs_individual_in_natal$FieldPeriodID == 33, "Y",
                                  ifelse(FPIDs_individual_in_natal$FieldPeriodID == 119, "Y", "N"))

translocs_Y <- translocs[(translocs$Translocation=="Y"),]

#How many translocation events have they lived through
translocs_number <- table(translocs_Y$BirdID)
translocs_number2 <- as.data.frame(translocs_number)
translocs_number2 <- translocs_number2 %>%
  rename(BirdID = Var1,
         NoTranslocationEvents = Freq) %>%
  mutate(TranslocationYN = "Y")



# 4. Merge birds that lived through translocs with those that didn't --------

no_translocation <- anti_join(personality, translocs_Y, 'BirdID')

no_translocation <- no_translocation %>%
  select(BirdID) %>%
  mutate(NoTranslocationEvents = NA,
         TranslocationYN = "N")

no_translocation$BirdID <- as.factor(no_translocation$BirdID)

translocation <- rbind(translocs_number2, no_translocation)


#Make manual amendment to BirdID 5534
translocation$TranslocationYN <- ifelse(translocation$BirdID == 5534, "Y", translocation$TranslocationYN)
translocation$NoTranslocationEvents <- ifelse(translocation$BirdID == 5534, 1, translocation$NoTranslocationEvents)

#Save
write.csv(translocation, "8_Translocation/Translocation.csv", row.names = FALSE)


















