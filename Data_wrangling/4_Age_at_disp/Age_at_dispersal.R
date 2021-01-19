#Code to calculate the age of dispersal for all individuals with personality scores prior to dispersal


# Read in data ------------------------------------------------------------

rm(list = ls())
library("ggplot2")
library("dplyr")
library("lubridate")
library("magrittr")

getwd()
setwd("Data_wrangling")

#tblBirdID.csv is the whole table taken from the database
birth_date <- read.csv('DB_tables_queries/tblBirdID.csv')
#Subset to required data
birth_date2 <- subset(birth_date, select=c('BirdID', 'BirthDate'))

#Data produced by R script 'Identifying_dispersers_and_philos_final_FPID_sub'
disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")


#Check data
str(birth_date2)
str(disp_philo)




# Create df of dispersers: containing their birth and disperse dates -------------------------------------

#Subset to dispersers
disp_dates <- disp_philo %>%
  filter(Disperse == 1) %>%
  select(c('BirdID', 'Disperse', 'DispersalDate', 'FieldPeriodID')) #select columns needed to calculate the difference between birth and disperse date


#Add birth date
disp_birth_dates <- merge(disp_dates, birth_date2, by='BirdID')


#Add a new column for the age at dispersal, this will be calculated and autofilled later
disp_birth_dates["Age_at_disp"] <- NA


#Change the format of BirthDate to match PeriodStart date, so they can be compared later
disp_birth_dates$BirthDate <- as.Date(disp_birth_dates$BirthDate , "%d/%m/%Y")




# Calculate and plot age at dispersal (in days) ---------------------------

#Calculate difference between birth and dispersal date (in days) and the mean
disp_birth_dates2 <- disp_birth_dates
disp_birth_dates2$Age_at_disp <- difftime(disp_birth_dates2$DispersalDate, disp_birth_dates2$BirthDate, units = c("days"))
mean(disp_birth_dates2$Age_at_disp)
range(disp_birth_dates2$Age_at_disp)



#Plot data
a <- ggplot(disp_birth_dates2, aes(x=Age_at_disp)) 
a <- a + theme_minimal() +
  geom_histogram(binwidth=50,color="black",boundary = 0, closed = "left") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = mean(Age_at_disp)), linetype = "dashed", size = 0.6)
a


#Subset so it can be merged with philopatrics later in the script
age_at_disp <- subset(disp_birth_dates2, select=c('BirdID', 'Age_at_disp', 'DispersalDate', 'BirthDate'))




# Create a subset that removes individuals that dispersed at <6 mo --------
adjust_age_at_disp <- age_at_disp %>% filter(!(Age_at_disp < 150))

yng.dispersers <- age_at_disp %>% filter((Age_at_disp < 150))




# Use second method to calculate birth date -------------------------

#First (above) method uses estimated birth date for birth date
#This method will use the hatch date of an UR chick born in the birth field period of that focal individual


#First, subset to list of birds with estimated birth dates and FPIDs
birth_date3 <- birth_date %>% select(BirdID, BirthDate, BirthFieldPeriodID, BirthDateEstimated)

disp_IDs <- disp_philo %>% select(BirdID, NatalTerritory)

disp_birth_est <- merge(disp_IDs, birth_date3, by=c('BirdID'))

disp_birth_estT <- disp_birth_est %>% 
  filter(!(BirthDateEstimated == "FALSE")) %>% #remove birds that have accurate birth dates
  select(!(BirthDateEstimated))



#Next, identify if there is a UR FL in natal territory during birth FPID 
#Do this by checking whether there were any nests that produced a FL in natal territory during birth FPID
nests <- read.csv('DB_tables_queries/qry_NestInfoWithTerrID.csv')
nests$HatchDateLatest <- as.Date(nests$HatchDateLatest , "%d/%m/%Y")
nests$FledgeDateLatest <- as.Date(nests$FledgeDateLatest , "%d/%m/%Y")

nests.fl <- nests %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>% #fill blank cells with NAs
  filter(!(is.na(FledgeDateLatest))) %>% #remove NAs i.e. nests that didnt produce FLs
  rename(NatalTerritory = TerritoryID, BirthFieldPeriodID = FieldPeriodID)

#Merge with disperse data
disp_birth_nests <- merge(disp_birth_estT, nests.fl, by = c('NatalTerritory', 'BirthFieldPeriodID'))


#Remove any nests that produced a known chick that wasn't the focal bird
chick <- read.csv('DB_tables_queries/tblChickInfo.csv') #read in all chick data 
chick %<>% select(BirdID, NestID, HatchDate) %>%
  rename(ChickHatchDate = HatchDate, ChickID = BirdID)


disp_nest_CH <- merge(disp_birth_nests, chick, by = c('NestID'), all.x = TRUE) #merge by nest ID to see whether the nest in focal birds natal territory during birth FPID had a rung chick
disp_nest_noCH <- disp_nest_CH %>% 
                  filter(is.na(ChickID) | BirdID == ChickID) %>% #remove if chick was present isn't focal bird 
                  mutate(HatchDateLatest = as.Date(ifelse(is.na(HatchDateLatest), FledgeDateLatest - days(14), HatchDateLatest), origin="1970-01-01")) #if hatch date is missing, take fledge date and minus 14 days 
  
 
  
#Remove nests with a hatch date later than first catch date
catch <- read.csv('DB_tables_queries/qry_CatchesWithDate.csv')
catch$OccasionDate <- as.Date(catch$OccasionDate , "%d/%m/%Y")

catch_filt <- catch %>%
  select(BirdID, OccasionDate)%>%
  group_by(BirdID) %>%
  slice(which.min(OccasionDate))

disp_nests_hatch <- merge(disp_nest_noCH, catch_filt, by = c('BirdID'))
disp_nests_hatch2  <- disp_nests_hatch %>% filter(!(HatchDateLatest > OccasionDate))


#Make manual edits - for birds that have two nests in natal terr, check which one is more likely to be the correct nest, or remove both if neither are suitable
disp_nests_corr <- disp_nests_hatch2 %>%
                   filter(!(BirdID == 5517 | BirdID == 6213 | 
                            BirdID == 5782 & NestNumber == 2 |
                            BirdID == 6018 & NestNumber == 1 | 
                            BirdID == 5498 & NestNumber == 2 |
                            BirdID == 5530 & NestNumber == 1)) %>%
                   select(BirdID, HatchDateLatest) %>%
                   rename(BirthDate2 = HatchDateLatest)
                   
#Merge with original birth estimate data 
birth_est1 <- disp_birth_est %>%        
      select(BirdID, BirthDate, BirthDateEstimated) %>% 
      rename(BirthDate1 = BirthDate) %>%
      mutate(BirthDate1 = as.Date(BirthDate1, format = "%d/%m/%Y"))

both_estimates <- merge(birth_est1, disp_nests_corr, by = c('BirdID'), all.x = TRUE) 

both_estimates2 <- both_estimates %>%
                   mutate(BirthDate2 = as.Date(ifelse(BirthDateEstimated == "FALSE", BirthDate1, BirthDate2), origin="1970-01-01")) %>%
                   mutate(BirthDate2 = as.Date(ifelse(is.na(BirthDate2), BirthDate1, BirthDate2), origin="1970-01-01")) %>%
                   select(!(BirthDateEstimated))
                            
    
#Re-add disperse date to df
birth_est_disp <- merge(both_estimates2, disp_dates, by = c('BirdID'), all.x = TRUE)
birth_est_disp %<>% mutate(Disperse = ifelse(is.na(Disperse), 0, Disperse))


#Calculate age at dispersal based on BirthDate1 and BirthDate2
birth_est_disp$Age_at_disp1 <- difftime(birth_est_disp$DispersalDate, birth_est_disp$BirthDate1, units = c("days"))
birth_est_disp$Age_at_disp2 <- difftime(birth_est_disp$DispersalDate, birth_est_disp$BirthDate2, units = c("days"))



birth_est_disp1 <- birth_est_disp %>% filter(Disperse == 1)

mean(birth_est_disp1$Age_at_disp1)
range(birth_est_disp1$Age_at_disp1)

mean(birth_est_disp1$Age_at_disp2)
range(birth_est_disp1$Age_at_disp2)




#Plot data
a <- ggplot(birth_est_disp1, aes(x=Age_at_disp1)) 
a <- a + theme_minimal() +
  geom_histogram(binwidth=50,color="black",boundary = 0, closed = "left") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = mean(Age_at_disp1)), linetype = "dashed", size = 0.6)
a


b <- ggplot(birth_est_disp1, aes(x=Age_at_disp2)) 
b <- b + theme_minimal() +
  geom_histogram(binwidth=50,color="black",boundary = 0, closed = "left") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = mean(Age_at_disp2)), linetype = "dashed", size = 0.6)
b




# Remove philos under mean disp age for each disp age ---------------------------------------
#Philopatrics that are below the mean age of dispersal may only be philopatrics because they are young and haven't reached the right age of dispersal (in case age is a driver of dispersal), rather than remaining philopatric because they are not exploratory
#Therefore, determine 95%CI and remove any philos that are below lower bound of mean age at dispersal

#Determine 95% CI 
a1 <- mean(birth_est_disp1$Age_at_disp1)
s1 <- sd(birth_est_disp1$Age_at_disp1)
n1 <- 90
error <- qnorm(0.975)*s1/sqrt(n1)
(lower1 <- a1 - error)
(upper1 <- a1 + error)

#Repeat for age_at_disp2
a2 <- mean(birth_est_disp1$Age_at_disp2)
s2 <- sd(birth_est_disp1$Age_at_disp2)
n2 <- 90
error <- qnorm(0.975)*s2/sqrt(n2)
(lower2 <- a2 - error)
(upper2 <- a2 + error)





#If age of philopatric are below the upper bound, remove from dataset

#retrieve philo data
philo <- birth_est_disp %>%
  filter(Disperse == 0) 


#determine how old they are during the last FPID they were seen 
#find every status for every individual
status <- read.csv("DB_tables_queries/sys_BreedGroupLocation+Status.csv")
FPID <- read.csv("DB_tables_queries/tblFieldPeriodIDs.csv")

status_dates <- merge(status, FPID, by= c('FieldPeriodID'))
status_dates$PeriodEnd <- as.Date(status_dates$PeriodEnd, "%d/%m/%Y")


#subset status list to retrieve the last field period each bird received a status
status_latest <- status_dates %>%
  filter(!(Status == "NS" | Status == "NSA")) %>% #remove any not seen statuses (NSA and NS)
  group_by(BirdID) %>%
  arrange(PeriodEnd) %>%
  filter(PeriodEnd == max(PeriodEnd)) %>% #retrieve the latest status/fieldperiod for each individual
  select(BirdID, PeriodEnd) %>% #subset to PeriodEnd, this is the last possible date the bird could have been seen
  rename(Date_last_seen = PeriodEnd) %>%
  distinct() #subset to unique records as some birds had multiple statuses within the fieldperiod


#merge with philo data
date_last_seen <- merge(philo, status_latest, by =c('BirdID'))


#calculate age last seen on Cousin for both birth dates 
date_last_seen$Age_last_seen1 <- difftime(date_last_seen$Date_last_seen, date_last_seen$BirthDate1, units = c("days"))
date_last_seen$Age_last_seen2 <- difftime(date_last_seen$Date_last_seen, date_last_seen$BirthDate2, units = c("days"))


#remove individuals that have not reached upper 95% CI - age_last_seen1 and age_last_seen2 
older1 <- date_last_seen %>% filter(Age_last_seen1 > upper1)
younger1 <- date_last_seen %>% filter(Age_last_seen1 < upper1)

older2 <- date_last_seen %>% filter(Age_last_seen2 > upper2)
younger2 <- date_last_seen %>% filter(Age_last_seen2 < upper2)








# Save --------------------------------------------------------------------

#File to contain all birds tested for personality
#Indicating estimated BirthDate1 (taken from database) and BirthDate2 (estimated using a combination of BirthDate1 and nest hatch dates)
#As well as Age_at_disperal, calculated from using BirthDate1 (Age_at_dispersal1) and BirthDate2 (Age_at_dispersal2)
#And whether philopatrics were old enough to make the dataset depending on the upper 95%CI of mean from Age_at_dispersal1/2


older1$Reached_min_age_1 <- 1
older2$Reached_min_age_2 <- 1 

older <- merge(older1, older2, by=c('BirdID', 'BirthDate1', 'BirthDate1', 'BirthDate2', 
                           'Disperse', 'DispersalDate', 'FieldPeriodID', 'Age_at_disp1', 
                           'Age_at_disp2', 'Date_last_seen', 'Age_last_seen1', 'Age_last_seen2'), all = TRUE)

older %<>% 
  mutate(Reached_min_age_1 = ifelse(is.na(Reached_min_age_1), 0, Reached_min_age_1)) %>%
  mutate(Reached_min_age_2 = ifelse(is.na(Reached_min_age_2), 0, Reached_min_age_2)) %>%
  select(!(Date_last_seen)) %>%
  select(!(Age_last_seen1)) %>% 
  select(!(Age_last_seen2))
  

#Merge with dispersal data
birth_est_disp1$Reached_min_age_1 <- NA #add column so df can rbind with philo data
birth_est_disp1$Reached_min_age_2 <- NA


older_disp_phi <- rbind(older, birth_est_disp1) #rbind with dispersal df


#Add year of birth and year of dispersal as they are required as fixed and random effects for model
#Add year of birth and year of dispersal as they are required as fixed and random effects for model
older_disp_phi$DispersalDate <- as.Date(older_disp_phi$DispersalDate, "%Y-%m-%d") 
older_disp_phi$DispersalYear <- as.numeric(format(older_disp_phi$DispersalDate,'%Y'))
older_disp_phi$BirthYear1 <- as.numeric(format(older_disp_phi$BirthDate1,'%Y'))
older_disp_phi$BirthYear2 <- as.numeric(format(older_disp_phi$BirthDate2,'%Y'))

older_disp_phi %<>% select(BirdID, Age_at_disp1, Age_at_disp2, DispersalDate, BirthDate1, BirthDate2, Reached_min_age_1, Reached_min_age_2)




write.csv(older_disp_phi, '4_Age_at_disp/Age_at_dispersal_older.csv')





# Save ALL without extra info on reaching minimum date --------------------------------------------------

#File including all philopatrics, regardless of age last seen
all <- birth_est_disp
all %<>% select(BirdID, Age_at_disp1, Age_at_disp2, DispersalDate, BirthDate1, BirthDate2, Disperse)


#Add whether they reached min age at dispersal
min_age <- older_disp_phi %>% select(BirdID, Reached_min_age_1, Reached_min_age_2)
all_min_age <- merge(all, min_age, by = c('BirdID'), all.x = TRUE)

all_min_age2 <- all_min_age %>% 
  mutate(Reached_min_age_1 = ifelse(is.na(Reached_min_age_1), 0, Reached_min_age_1)) %>%
  mutate(Reached_min_age_1 = ifelse(Disperse == 1, NA, Reached_min_age_1)) %>%
  mutate(Reached_min_age_2 = ifelse(is.na(Reached_min_age_2), 0, Reached_min_age_2)) %>%
  mutate(Reached_min_age_2 = ifelse(Disperse == 1, NA, Reached_min_age_2))
  




#Add year of birth and year of dispersal as they are required as fixed and random effects for model
all_min_age2$DispersalDate <- as.Date(all_min_age2$DispersalDate, "%Y-%m-%d") 
all_min_age2$DispersalYear <- as.numeric(format(all_min_age2$DispersalDate,'%Y'))
all_min_age2$BirthYear1 <- as.numeric(format(all_min_age2$BirthDate1,'%Y'))
all_min_age2$BirthYear2 <- as.numeric(format(all_min_age2$BirthDate2,'%Y'))


all_min_age3 <- all_min_age2 %>% select(-c(DispersalDate, Disperse))


#Save
write.csv(all_min_age3, '4_Age_at_disp/Age_at_dispersal_all.csv', row.names = FALSE)

