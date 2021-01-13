
#This script:
#a) Identifies dispersers and philopatrics 
#b) Determines their dispersal dates using one of three methods:
#     i) Within-season dispersal
#     ii) Between-season dispersal
#     iii) Floaters dispersal 
#c) Add natal territory data for dispersers and philopatrics, including:
#     i) Last field period spent in natal territory (plus period start+end)
#     ii) Second last field period in natal territory (needed for future fixed effects scripts where no data were collected during last season in natal terr) (plus period start+end)
#     iii) Natal territory ID
#     iv) Last known status in natal territory 
#     v) Whether status in new terr was dominant(1) or subordinate(2)
#d) Identify philopatrics, their natal territory and the season they were last seen in their natal territory




# Retrieve data & load packages -------------------------------------------

rm(list = ls())

getwd()

setwd("Data_wrangling")

library(tibble)
library(dplyr)
library(tidyverse)
library(magrittr)


#Read in data produced by running 'Collating_dispersal_data.R' - this includes every territory each individual has resided in
terr_dates <- read.csv('Pedigree_database_terri_merged.csv')


#Check and correct data format
str(terr_dates)

terr_dates$PeriodStart <- as.Date(terr_dates$PeriodStart , "%d/%m/%Y")
terr_dates$PeriodEnd <- as.Date(terr_dates$PeriodEnd , "%d/%m/%Y")
terr_dates$BirdID <- as.factor(terr_dates$BirdID) 
terr_dates$FieldPeriodID <- as.factor(terr_dates$FieldPeriodID) 
terr_dates$TerritoryID <- as.factor(terr_dates$TerritoryID)


#Identify individuals that were cross fostered ('XF' indicates bird was moved INTO the associated territory ID)
#Rename XF to CH
#Remove their old territory ID (as they have been moved to the new XF territory)
XF <- terr_dates %>%
  filter(Status == "XF") %>%
  select(BirdID) %>%
  distinct()


XF3 <- terr_dates %>%
  group_by(BirdID, Status) %>%
  filter(!(BirdID %in% XF$BirdID && Status == "CH")) %>%
  ungroup() %>%
  mutate(Status = ifelse(Status == "XF", "CH", Status)) %>%
  filter(!(BirdID == 1042 & TerritoryID == 171)) #remove this row manually - bird was XF twice in same season, no way of automating removal of the first XF terr as they are both in same FP
 

  



# Identify dispersers -------------------------------------------------
#I want to obtain the first two territories an individual resided in *i.e. natal territory and 
#dispersal territory*. For now, I do not needed to know what territories an individual dispersed 
#to following their initial dispersal event. 

#Group by BirdID, then arrange entries for each BirdID by ascending date 
terr_datesnoXF <- XF3 %>%
  arrange(
    BirdID,
    (PeriodStart)
  )


#Identify any individuals that have resided in the same territory for multiple seasons and
#subset to earliest date an individual resided there
terr_no_dups <- terr_datesnoXF[!duplicated(terr_datesnoXF[,c('BirdID', 'TerritoryID')]),]


#Remove all individuals that appear less than twice i.e. philopatric individuals
#Disperse column value '1' indicates the individual has dispersed
dispersers <-  terr_no_dups[!table(terr_no_dups$BirdID)[as.character(terr_no_dups$BirdID)] <2,] 
dispersers$Disperse <- '1'




#Extract the second territory the individual was resident in (this is the territory they dispersed to)
disp_terr <- dispersers %>%
  group_by(BirdID) %>%
  arrange(PeriodStart) %>%
  slice(2)


#Double check there are no conflicting records for juv statuses e.g. FL in one terr but OFL in different terr
#These should have been corrected in script 'Collecting_dispersal_data.R'
conflicts <- disp_terr
conflicts <- subset(conflicts, Status == "CH" | Status == "FL" | Status == "OFL")
conflicts





# Estimate dispersal date pt. 1 - Read in sightings -------------------------------------------------

#First read in list of sightings (observations, catches and nest watches) to determine when individuals were first observed in their new territory

#Read in observation data, subset to required data and rename columns to match column names used in 'dispersers' df
#csv produced from 'qry_ObservationDetailsPersonality', which collated observation tables and subsets to only observations of individuals with personality scores
obs <- read.csv('DB_tables_queries/qry_ObservationDetails.csv')


obs_core <- obs %>% 
    select(-c(ObservationID, Remarks)) %>% 
    dplyr::rename(
    FieldPeriodID = ObservationFieldPeriodID,
    Date = ObservationDate
  )


#Read in catch data, subset to required data and rename columns to match column names used in dispersers df
#csv produced by 'qry_CatchesWithDate'
catch <- read.csv('DB_tables_queries/qry_CatchesWithDate.csv')


catch_core <- catch %>% 
  select(c(BirdID, FieldPeriodID, OccasionDate, TerritoryID)) %>%
  dplyr::rename(Date = OccasionDate)


#Read in nest watch data, subset to required data and rename columns to match column names used in dispersers df
#csv produced by qry_NestWatchesWithDate
nw <- read.csv('DB_tables_queries/qry_NestWatchesWithDate.csv')


nw_core <- nw %>% 
  select(c(BirdID, FieldPeriodID, WatchDate, TerritoryID)) %>%
  dplyr::rename(Date = WatchDate)


#Merge obs, catch and nest watch data + reformat
sightings_all <- rbind(obs_core, catch_core, nw_core) 

str(sightings_all)


sightings_all$BirdID <- as.factor(sightings_all$BirdID)
sightings_all$FieldPeriodID <- as.factor(sightings_all$FieldPeriodID)
sightings_all$Date <- as.Date(sightings_all$Date, "%d/%m/%Y")
#sightings_all$TerritoryID <- as.factor(sightings_all$TerritoryID)

sightings_all <- sightings_all %>% 
  dplyr::rename(SightingTerritoryID = TerritoryID)


#Retrieve observations, catches and NWs for ONLY the field period an individual dispersed
#Note: the field period an individual dispersed is the field period they were first seen in their new territory
sightings_disps <- merge(disp_terr, sightings_all, by=c("BirdID", "FieldPeriodID"))


#Check how many of the dispersers have not been observed/caught during their dispersal field period, this should be none of them
#1494 and 2978 have no sightings during dispersal field period 
#1494 given status by DR despite no sightings, have checked his scanned field book but no entries for 1494
#2978 caught for the first time during the season AFTER dispersal (bird's natal territory from pedigree) - no observer for that season 
in.dispersers.not.sightings_disps <- anti_join(disp_terr, sightings_disps, by ='BirdID') 
in.dispersers.not.sightings_disps





# Estimate dispersal date pt. 2 - identify precise dates -------------------------------------------

#There are two categories of dispersal dates
#1) Precise dates - we know these dates because individuals dispersed within a field period, so we were there to record the event
#2) Between dates - as most individuals will disperse between field periods, we must estimate their dispersal date by using the median date between PeriodEnd for the FP last seen at the natal territory and PeriodStart at the new territory
#This section identifies individuals with precise dispersal dates


#First, identify natal territories
#Use 'Pedigree_database_terri_merged.csv' data
natal_territories <- terr_datesnoXF


#Reformat dates so R can read them in chronological order
natal_territories$PeriodStart <- as.Date(natal_territories$PeriodStart , "%d/%m/%Y")
natal_territories$PeriodEnd <- as.Date(natal_territories$PeriodEnd , "%d/%m/%Y")


#Take the earliest residency data for each individual, this will provide the first territory an individual ever resided in (i.e. natal territory)
#Rename natal territory column
natal_territories <- natal_territories %>% 
  group_by(BirdID) %>% 
  slice(which.min(PeriodStart)) %>%
  dplyr::rename(NatalTerritory = TerritoryID)


#Save natal territory data as it is needed for other scripts
write.csv(natal_territories, "2_Identifying_dispersers_and_philopatrics/Natal_territories.csv", row.names = FALSE)

natal <- subset(natal_territories, select=c(BirdID, NatalTerritory))
write.csv(natal, '2_Identifying_dispersers_and_philopatrics/Natal_terr.csv', row.names = FALSE)



#Check data and correct format
str(natal)

natal$BirdID <- as.factor(natal$BirdID)
#natal$NatalTerritory <- as.factor(natal$NatalTerritory)
#natal <- subset(natal, select = c(BirdID, NatalTerritory))


#Add natal territory data to sightings of dispersers df
sightings_disps_nat <- merge(natal, sightings_disps, by=c('BirdID'))


#Subset the df to individuals that have been observed in their natal territory during the dispersal FPID
#This indicates they did not disperse between field periods
seen_natal <- subset(sightings_disps_nat, NatalTerritory == SightingTerritoryID)


#Take the latest date an individual was seen on their natal territory during the dispersal FPID
last_seen_natal <- seen_natal %>% 
  group_by(BirdID) %>% 
  slice(which.max(Date)) %>%
  rename(Last.seen.on.natal.terr = Date)

last_seen_natal$BirdID <- as.factor(last_seen_natal$BirdID)

last_seen_natal <- subset(last_seen_natal, select = -c(SightingTerritoryID))


#Take the first date an individual was seen in their new territory during the dispersal FPID
first_seen_new <- subset(last_seen_natal, select = c(BirdID))

first_seen_new <- merge(first_seen_new, sightings_disps_nat)

first_seen_new <- subset(first_seen_new, TerritoryID == SightingTerritoryID)

first_seen_new <- first_seen_new %>% 
  group_by(BirdID) %>% 
  slice(which.min(Date)) %>%
  rename(First.seen.on.new.terr = Date)

first_seen_new <- subset(first_seen_new, select = -c(SightingTerritoryID))


#Merge the two 
seen_natal_all <- merge(last_seen_natal, first_seen_new, 
                        select=c('BirdID', 'NatalTerritory', 'FieldPeriod', 'TerritoryID',
                                 'Status', 'PeriodStart', 'PeriodEnd', 'Disperse'), all = TRUE)



#Calculate mid-point between date last seen in natal terr and date first seen in new terr
seen_natal_all$DispersalDate <- seen_natal_all$Last.seen.on.natal.terr + 
  floor((seen_natal_all$First.seen.on.new.terr-seen_natal_all$Last.seen.on.natal.terr)/2)






##########################
#Alternatively - dispersal date could be the date of arrival in the new territory 

#To use date of arrival instead of mid-point, rename First.seen.on.new.terr as DispersalDate

#seen_natal_all <- subset(seen_natal_all, select = -c(DispersalDate))

#seen_natal_all <- seen_natal_all %>%
#rename(DispersalDate = First.seen.on.new.terr)
#########################





#Check all individuals seen in their natal territory during the dispersal FPID have an observation in their new territory
#If not, this may be because they are floaters. A floater's default dispersal territory is '-3' (means no resident territory/NA). As an individual
#will not be sighted in a non-existing -3 territory, they will be missed from this df
(missing.new.terr.obs <- seen_natal_all[is.na(seen_natal_all$First.seen.on.new.terr),])


#If all individuals are floaters, place them in a new dataframe called FLOAT
#This df will be used later 
if(length(unique(missing.new.terr.obs[,5]))==1) {
  FLOAT_1 <- missing.new.terr.obs
}


#Remove floaters from 'seen_natal_all' as they will have their dispersal date calculated separately below
seen_natal_all <- seen_natal_all[!(seen_natal_all$Status=="FLOAT"),]





# Estimate dispersal date pt. 3 - identify between dates -------------------------------------------

#As we cannot determine an exact date of dispersal, we will need to find the median date between the field period at the natal and new territory
#Anti-join to determine which individuals were not seen in their natal territory

seen_natal_ID <- subset(seen_natal_all, select = c(BirdID))

#There should be a line of code that merges seen_natal_all with floaters to ensure floaters aren't included following the below antijoin?

(between <- anti_join(disp_terr, seen_natal_ID, by='BirdID') %>% select(BirdID))



#Merge df of 'between' individuals with their natal territory and dispersal data
between_1 <- merge(between, natal, by= c('BirdID'))
between_2 <- merge(between_1, disp_terr, by= c('BirdID'))


between_2 <- between_2 %>% 
  dplyr::rename(DisperseFPID = FieldPeriodID,
                DisperseStatus = Status,
                DispersePeriodStart = PeriodStart,
                DispersePeriodEnd = PeriodEnd,
                DisperseTerritoryID = TerritoryID)


between_2$BirdID <- as.factor(between_2$BirdID)


#Copy df created earlier in section 3 'terr_datesnoXF', it contains every territory an individual has ever resided in
status <- terr_datesnoXF

str(status)


status <- status %>% 
  dplyr::rename(StatusTerritoryID = TerritoryID)


#Merge disperser df with statuses
between_statuses <- merge(between_2, status, by = c('BirdID'))


#Remove any statuses obtained after the dispersal field period
between_pre_statuses <- between_statuses[!(between_statuses$PeriodEnd>between_statuses$DispersePeriodStart),]
#between_pre_statuses$BirdID <- as.factor(between_pre_statuses$BirdID)


#Identify FP individual was last seen in natal territory
#Convert territory IDs to characters as below script does not work for factors with different numbers of levels
between_pre_statuses$NatalTerritory <- as.character(between_pre_statuses$NatalTerritory)
between_pre_statuses$StatusTerritoryID <- as.character(between_pre_statuses$StatusTerritoryID)
between_pnatal_statuses <- subset(between_pre_statuses, NatalTerritory == StatusTerritoryID)


#Identify the latest date an individual was seen to have a status in their natal territory
latest_between_natal_statuses <- between_pnatal_statuses %>% 
  group_by(BirdID) %>% 
  slice(which.max(PeriodEnd))


#Calculate midpoint between PeriodEnd and DispersePeriodStart, this will be the dispersal date
latest_between_natal_statuses$DispersalDate <- latest_between_natal_statuses$PeriodEnd + 
  floor((latest_between_natal_statuses$DispersePeriodStart-latest_between_natal_statuses$PeriodEnd)/2)


#Remove incorrect floater calculations
latest_between_natal_statuses <- latest_between_natal_statuses[!(latest_between_natal_statuses$DisperseStatus=='FLOAT'),]





# Estimating dispersal date pt.4 - floater's dispersal dates --------------


#Due to the uncertain nature of floaters, they will have dispersal date calculated 
#as the median date between the the FPID they were last seen as a floater and FPID they 
#obtained a permanent group position/status

#First, identify the last FPID an individual was a floater

#Create list of floaters
FLOAT_IDs_1 <- subset(FLOAT_1, select = c('BirdID'))
FLOAT_IDs_2 <- between_2[(between_2$DisperseStatus == 'FLOAT'),]
FLOAT_IDs_2 <- subset(FLOAT_IDs_2, select = c('BirdID'))

FLOAT_IDs <- rbind(FLOAT_IDs_1, FLOAT_IDs_2)


#Merge with every territory they have resided in, then subset to only when they were floaters
float_terrs <- merge(FLOAT_IDs, terr_datesnoXF, select = c('BirdID'))
float_FPIDs <- float_terrs[(float_terrs$Status == "FLOAT"),]

#Subset to last season individual was a floater
float_FPIDs <- float_FPIDs %>% 
  group_by(BirdID) %>% 
  arrange(PeriodStart, .by_group = TRUE) %>%
  slice(which.max(PeriodEnd)) %>%
  rename(FloatPeriodStart = PeriodStart,
         FloatPeriodEnd = PeriodEnd,
         FloatFPID = FieldPeriodID)



#Identify the FPID the individual settled after floating

#Create list of floaters and every territory they have resided in 
post_float_FPIDs <- subset(float_FPIDs, select = -c(Status, TerritoryID))
post_float_FPIDs <- merge(post_float_FPIDs, terr_datesnoXF, by = c('BirdID'))

#Remove any territories the individual resided in before becoming a floater, as well as any FPIDs they were a floater
post_float_FPIDs$FPID.prior.or.after.floating <- ifelse(post_float_FPIDs$PeriodEnd<post_float_FPIDs$FloatPeriodEnd, 1,
                                                        ifelse(post_float_FPIDs$PeriodEnd==post_float_FPIDs$FloatPeriodEnd, 1,
                                                               ifelse(post_float_FPIDs$PeriodEnd>post_float_FPIDs$FloatPeriodEnd, 0, NA)))

post_float_FPIDs <- post_float_FPIDs[(post_float_FPIDs$FPID.prior.or.after.floating==0),]

#Take the earliest FPID an individual was recorded after they were floaters
#Some individuals (5781, 5907, 6046, 6137) will be lost as they were NSA after being floaters
post_float_FPIDs <- post_float_FPIDs %>% 
  group_by(BirdID) %>% 
  slice(which.min(PeriodEnd)) 


#Calculate mid point between when an individual was last seen as a floater and first seen in new territory
post_float_FPIDs$DispersalDate <- post_float_FPIDs$FloatPeriodEnd + 
  floor((post_float_FPIDs$PeriodStart-post_float_FPIDs$FloatPeriodEnd)/2)


float_dispersers <- subset(post_float_FPIDs, select = -c(FloatFPID, 
                                                         FloatPeriodStart, FloatPeriodEnd, FPID.prior.or.after.floating))


#Add disperse=1 column
float_dispersers$Disperse = 1





# Estimate dispersal date pt. 5 - merge -----------------------------------

#Format columns of the precise df 'seen_natal_all', estimate df 'latest_between_natal_statuses' and floater df 'float_dispersrs' to match, so I can rbind them
float_dispersers <- float_dispersers[, c(1, 2, 3, 4, 5, 6, 8, 7)]

seen_natal_all <- seen_natal_all[, c(1, 4, 5, 3, 6, 7, 8, 11)]

latest_between_natal_statuses <- latest_between_natal_statuses[, c(1, 3, 4, 5, 6, 7 , 8, 14)]

latest_between_natal_statuses <- latest_between_natal_statuses %>% 
  dplyr::rename(FieldPeriodID = DisperseFPID,
                Status = DisperseStatus,
                PeriodStart = DispersePeriodStart,
                PeriodEnd = DispersePeriodEnd,
                TerritoryID = DisperseTerritoryID)


#Add column to indicate whether dispersal date was calculated using precise 'P' or estimate 'E' method
seen_natal_all$Method = "P"
latest_between_natal_statuses$Method = "E"
float_dispersers$Method = "EF"


#rbind
dispersal_dates <- rbind(seen_natal_all, latest_between_natal_statuses)


#Are there any missing that were in the original list of dispersers
where_dey_go <- anti_join(float_dispersers, disp_terr, dispersal_dates, by='BirdID') %>% select(BirdID)
where_dey_go


#Add column 'Prior_dispersal' which means: was the individual tested for personality prior to dispersal (0=N, 1=Y)
dispersal_dates$Prior_dispersal <- "UNK"


#Identify whether dispersal season was winter or summer
#Include only month
dispersal_dates$DispersalMonth <- format(dispersal_dates$DispersalDate, "%m")

#Remove the 0 prior to month e.g. 07 to 7
dispersal_dates$DispersalMonth<- gsub("(?<![0-9])0+", "", dispersal_dates$DispersalMonth, perl = TRUE)

#Change month from numeric to string
dispersal_dates$DispersalMonth <- as.numeric(dispersal_dates$DispersalMonth)
dispersal_dates$DispersalMonth <- month.abb[dispersal_dates$DispersalMonth]

#Add column for whether FPID was winter or summer
dispersal_dates$DispersalMonth2 <- ifelse(dispersal_dates$DispersalMonth == "Jan", "W",
                                          ifelse(dispersal_dates$DispersalMonth == "Feb", "W",
                                                 ifelse(dispersal_dates$DispersalMonth == "Mar", "W",
                                                        ifelse(dispersal_dates$DispersalMonth == "Apr", "W",  
                                                               ifelse(dispersal_dates$DispersalMonth == "Oct", "W",    
                                                                      ifelse(dispersal_dates$DispersalMonth == "Nov", "W", 
                                                                             ifelse(dispersal_dates$DispersalMonth == "Dec", "W","S")))))))







# Add natal territory details for dispersers ------------------------------

## First, identify FPID last seen in natal territory 
all_status <- terr_datesnoXF %>%
  rename(NatalTerritory = TerritoryID)

all_status_natal <- merge(all_status, natal, by = c('BirdID', 'NatalTerritory'))

last_status_natal <- all_status_natal %>%
  group_by(BirdID) %>%
  arrange(PeriodEnd, .by_group = TRUE) %>%
  slice(which.max(PeriodEnd)) %>%
  rename(LastFPIDNatal = FieldPeriodID,
         LastFPIDNatalPeriodStart = PeriodStart,
         LastFPIDNatalPeriodEnd = PeriodEnd,
         NatalStatus = Status)


#Subset to just dispersers
final_status_natal_disp <- merge(dispersal_dates, last_status_natal, by = c('BirdID'), all.x = TRUE)


#Group levels and provide a code to dominant (1) or subordinates (2)
#Note - I have placed budder (B) into subordinate category as the budding did not appear successful (more likely a budding attempt)
#It is not shown on any of the new maps and not given a new territory ID e.g. 4.1. Therefore, classed as subordinate in their natal territory (4)
final_status_natal_disp$NatalStatus2 <- ifelse(final_status_natal_disp$NatalStatus == "BrM",1,
                                               ifelse(final_status_natal_disp$NatalStatus == "BrF",1,
                                                      ifelse(final_status_natal_disp$NatalStatus == "TBRM",1,2))) 


#Identify whether the individual dispersed and gained a dominant or subordinate position
final_status_natal_disp$Status2 <- ifelse(final_status_natal_disp$Status == "FLOAT",3,
                                          ifelse(final_status_natal_disp$Status == "H",2,
                                                 ifelse(final_status_natal_disp$Status == "AB",2, 
                                                        ifelse(final_status_natal_disp$Status == "ABX",2,1))))


#Subset to only individuals that have dispersed from subordinate breeding positions, as dispersal from a dom position is breeding, rather than natal, dispersal
final_status_disp_dom <- final_status_natal_disp[(final_status_natal_disp$NatalStatus2==2),]


#Add second last FPID a disperser was in their natal territory
#Read in record of all FPIDs, subset to required data and arrange in chronological order
FPID <- read.csv('DB_tables_queries/tblFieldPeriodIDs.csv')


#Remove FPID110 (25/11/11-24/12/11), FPID29 (20/11/1999-07/12/1999), FPID32 (17/05/2004-29/05/2004), FPID74 (15/03/2006-21/03/2006)
#As no island census was conducted these seasons (translocation seasons)
FPID_CN <- FPID %>%
  filter((Island == "CN")) %>% 
  filter(!(FieldPeriodID == "110" | FieldPeriodID == "29" | FieldPeriodID == "74" | FieldPeriodID == "32")) %>%
  select(c(FieldPeriodID, PeriodStart))

FPID_CN$PeriodStart <- as.Date(FPID_CN$PeriodStart , "%d/%m/%Y")
FPID_CN <- FPID_CN[order(as.Date(FPID_CN$PeriodStart)),]


#Check and correct FPID data formats
str(FPID_CN)
FPID_CN$FieldPeriodID <- factor(FPID_CN$FieldPeriodID)


#Add code for each FPID/row. As FPID numbers are not chronological, a chronological code will assist with 
#determining what the FPID is prior to dispersal
FPID_CN$FPIDCode <- 1:nrow(FPID_CN)


#Remove dates
FPID_CN_code <- subset(FPID_CN, select = c('FieldPeriodID', 'FPIDCode'))


#Rename FPID column to 'LastFPIDNatal' so it can be merged 
FPID_CN_code <- FPID_CN_code %>%
  rename(LastFPIDNatal = FieldPeriodID,
         LastFPIDCode = FPIDCode)


final_status_disp_dom <- merge(final_status_disp_dom, FPID_CN_code, by = c('LastFPIDNatal'))


#Create new column that will include FPIDCode for the second last season seen in natal terr
#Do this by copying the dispersal FPID and subtracting 1
SecondLastFPIDCode <- final_status_disp_dom$LastFPIDCode - 1
final_status_disp_dom <- cbind(final_status_disp_dom, SecondLastFPIDCode)


#Relabel the FPID dataframe from original 'FPIDCode' and 'LastNatalFPID' to match the column headings 
#in data frame all_sightings_natal. This allows me to merge the two dataframes (and add the FPID for the SecondLastFPIDCode)
FPID_CN_code <- FPID_CN_code %>%
  rename(SecondLastFPIDCode = LastFPIDCode,
         SecondLastFPIDNatal = LastFPIDNatal)

final_status_disp_dom <- merge(final_status_disp_dom, FPID_CN_code, by='SecondLastFPIDCode')


#Add dates for SecondLastFPIDNatal
FPID$PeriodStart <- as.Date(FPID$PeriodStart, "%d/%m/%Y")
FPID$PeriodEnd <- as.Date(FPID$PeriodEnd, "%d/%m/%Y")

FPID <- FPID %>%
  select(FieldPeriodID, PeriodStart, PeriodEnd) %>%
  rename(SecondLastFPIDNatal = FieldPeriodID,
         SecondLastFPIDNatalPeriodStart = PeriodStart,
         SecondLastFPIDNatalPeriodEnd = PeriodEnd)


final_status_disp_dom <- merge(final_status_disp_dom, FPID, by = c('SecondLastFPIDNatal'))

#Remove code columns
final_status_disp_dom <- select(final_status_disp_dom, -c('LastFPIDCode', 'SecondLastFPIDCode'))














# Identify philopatrics with dominant statuses  ---------------------------


##a. Remove any individual that has been consistently dominant throughout their lifetime
#Do this by checking whether their first status in their natal territory is dominant 

#First, remove any individuals that appear >1 in the df, *i.e. dispersers*
#*Note* 'terr_no_dups' df was created in section 2 'Identify dispersers', it contains the earliest date for every territory an individual has ever resided in
philos.all <- terr_no_dups[!table(terr_no_dups$BirdID)[as.character(terr_no_dups$BirdID)] >1,]


#Remove any individuals that do not have a subordinate position as their first known status i.e. remove dominants 
#This is because we don't have an accurate natal territory for these individuals 
philos <- philos.all[!(philos.all$Status == "BrF" | philos.all$Status == "BrM"),]


#Double check who had dominant statuses their whole life
(only.dominant.philos <- anti_join(philos.all, philos))




#b. Identify if philopatrics eventually gained a dominant position in their natal territory
#First, get list of every status for philopatric birds
philos.status <- subset(philos, select = c('BirdID'))
philos.status <- merge(philos.status, status, by =c('BirdID'))

#Code whether the status is for a dominant (2) or subordinate (1) position 
philos.status$Status.type <- ifelse(philos.status$Status == "BrF", 2,
                                    ifelse(philos.status$Status == "BrM", 2,1))

#Subset to every dominant status a philopatric has had
philos.dom <- philos.status[(philos.status$Status.type == "2"),]


#Remove incorrect status for BirdID 1660 (identified this manually)
philos.dom <- philos.dom[!(philos.dom$FieldPeriodID == 77 & philos.dom$BirdID == 1660),]


#Subset to earliest date an individual became a dominant
(philos.dom %<>% group_by(BirdID) %>% 
    slice(which.min(PeriodStart)))


#Identify the last FPIDs a disperser was a subordinate in their natal territory
FPID_CN_code %<>% rename(FieldPeriodID = SecondLastFPIDNatal,
                         FPIDCode = SecondLastFPIDCode)


philos.dom <- merge(philos.dom, FPID_CN_code, by = c('FieldPeriodID'))


#Create new column that will include FPIDCode for the last season seen as a subordinate in natal terr
#Do this by copying the first FPID as a dominant and subtracting 1
LastFPIDCode <- philos.dom$FPIDCode - 1
last_FPID_philos.dom <- cbind(philos.dom, LastFPIDCode)


#Relabel the FPID dataframe from original 'FPIDCode' and 'LastNatalFPID' to match the column headings 
#in data frame all_sightings_natal. This allows me to merge the two dataframes (and add the FPID for the SecondLastFPIDCode)
FPID_CN_code <- FPID_CN_code %>%
  rename(LastFPIDCode = FPIDCode,
         LastFPIDNatal = FieldPeriodID)

last_FPID_philos.dom <- merge(last_FPID_philos.dom, FPID_CN_code, by='LastFPIDCode')


#Add details for two seasons prior to becoming a dominant 


#Create new column that will include FPIDCode for the second last season seen as a subordinate in natal terr
#Do this by copying the LastFPIDCode and subtracting 1
SecondLastFPIDCode <- last_FPID_philos.dom $LastFPIDCode - 1
secondlast_FPID_philos.dom  <- cbind(last_FPID_philos.dom, SecondLastFPIDCode)

FPID_CN_code <- FPID_CN_code %>%
  rename(SecondLastFPIDCode = LastFPIDCode,
         SecondLastFPIDNatal = LastFPIDNatal)

secondlast_FPID_philos.dom  <- merge(secondlast_FPID_philos.dom, FPID_CN_code, by='SecondLastFPIDCode')


#Remove code columns
sub_to_dom <- secondlast_FPID_philos.dom %>%
  select('BirdID', 'StatusTerritoryID', 'LastFPIDNatal', 'SecondLastFPIDNatal') %>%
  rename(NatalTerritory = StatusTerritoryID)


sub_to_dom <- merge(sub_to_dom, FPID, by = c('SecondLastFPIDNatal'))


FPID <- FPID %>%
  rename(LastFPIDNatal = SecondLastFPIDNatal,
         LastFPIDNatalPeriodStart = SecondLastFPIDNatalPeriodStart,
         LastFPIDNatalPeriodEnd = SecondLastFPIDNatalPeriodEnd)


sub_to_dom <- merge(sub_to_dom, FPID, by = c('LastFPIDNatal'))


#Double check these seasons do not precede birth FPID
birdid <- read.csv("DB_tables_queries/tblBirdID.csv")

birdid <- birdid %>%
  select('BirdID', 'BirthDate')

birdid$BirthDate <- as.Date(birdid$BirthDate, "%d/%m/%Y")

sub_to_dom <- merge(sub_to_dom, birdid, by = c('BirdID'), all.x = TRUE)

sub_to_dom$FPID.before.birth <- ifelse(sub_to_dom$LastFPIDNatalPeriodEnd > sub_to_dom$BirthDate, "N", "Y")

sub_to_dom2 <- sub_to_dom[!(sub_to_dom$FPID.before.birth == "Y"),]

(sub_to_dom2 <- select(sub_to_dom2, -c('FPID.before.birth', 'BirthDate')))

sub_to_dom2$Inherit <- 1


##c. Identify philopatrics that have remained subordinates, then calculate LastFPIDNatal and SecondLastFPIDNatal

philos.sub <- anti_join(philos.status, sub_to_dom, by = c('BirdID'))

#Double check no dominant statuses 
(dom.errors <- philos.sub[(philos.sub$Status.type == 2),])


#Take latest date in natal territory - this is the LastFPIDNatal 
philos.sub.last <- philos.sub %>%
  group_by(BirdID) %>%
  slice(which.max(PeriodStart)) %>%
  select(-Status.type) %>%
  rename(NatalTerritory = StatusTerritoryID,
         LastFPIDNatal = FieldPeriodID,
         LastFPIDNatalPeriodStart = PeriodStart,
         LastFPIDNatalPeriodEnd = PeriodEnd)


#Identify SecondLastFPIDNatal
FPID_CN_code %<>%
  rename(LastFPIDNatal = SecondLastFPIDNatal,
         LastFPIDCode = SecondLastFPIDCode)

philos.sub.last <- merge(philos.sub.last, FPID_CN_code, by = c('LastFPIDNatal'))

SecondLastFPIDCode <- philos.sub.last$LastFPIDCode - 1

philos.sub.second.last <- cbind(philos.sub.last, SecondLastFPIDCode)

FPID_CN_code %<>%
  rename(SecondLastFPIDNatal = LastFPIDNatal,
         SecondLastFPIDCode = LastFPIDCode)

philos.sub.second.last <- merge(philos.sub.second.last, FPID_CN_code, by = c('SecondLastFPIDCode'))

#Remove codes
sub_always <- philos.sub.second.last %>%
  select(-'SecondLastFPIDCode', -'LastFPIDCode', -'Status')

#Add dates
FPID <- FPID %>%
  rename(SecondLastFPIDNatal = LastFPIDNatal,
         SecondLastFPIDNatalPeriodStart = LastFPIDNatalPeriodStart,
         SecondLastFPIDNatalPeriodEnd = LastFPIDNatalPeriodEnd)

sub_always <- merge(sub_always, FPID, by = c('SecondLastFPIDNatal'))


#Merge all philopatric data
sub_always <- sub_always[, c(3,2,1,4,7,8,5,6)]

sub_always$Inherit <- 0

philos.sorted <- rbind(sub_always, sub_to_dom2)


#Make manual change to BirdID6026'S natal territory - as their natal terr merged, the ID changed from 55 to 57.1 (82 to 85)
#philos.sorted[philos.sorted$BirdID==6026, "NatalTerritory"] <- 85


#An additional 'Disperse' column is made, with '0' indicating the individual has not dispersed.
philos.sorted["Disperse"] <- '0'


#Create columns needed to assist rbind with dispersal data at the end of the script
philos.sorted["Prior_dispersal"] <- NA
philos.sorted$Method <- NA
philos.sorted$DispersalDate <- NA
philos.sorted$DispersalDate <- as.Date(philos.sorted$DispersalDate, "%d/%m/%Y")
philos.sorted$DispersalMonth <- NA
philos.sorted$DispersalMonth2 <- NA
philos.sorted$Status2 <- NA
philos.sorted$FieldPeriodID <- NA
philos.sorted$PeriodStart <- NA
philos.sorted$PeriodStart <- as.Date(philos.sorted$PeriodStart, "%d/%m/%Y")
philos.sorted$PeriodEnd <- NA
philos.sorted$PeriodEnd <- as.Date(philos.sorted$PeriodEnd, "%d/%m/%Y")
philos.sorted$TerritoryID <- NA
philos.sorted$Status <- NA
philos.sorted$NatalStatus <- NA
philos.sorted$NatalStatus2 <- NA

final_status_disp_dom$Inherit <- NA

# Merge and save ----------------------------------------------------------

#Merge philopatric and dispersal data
disp_and_philo <- rbind(philos.sorted, final_status_disp_dom)


#Save
write.csv(disp_and_philo, "2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv", row.names = FALSE)
 

