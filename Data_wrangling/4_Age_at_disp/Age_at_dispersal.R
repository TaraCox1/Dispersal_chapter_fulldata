#Code to calculate the age of dispersal for all individuals with personality scores prior to dispersal


# Read in data ------------------------------------------------------------

rm(list = ls())
library("ggplot2")
library("dplyr")
getwd()
setwd("Data_wrangling")

#tblBirdID.csv is the whole table taken from the database
birth_date <- read.csv('DB_tables_queries/tblBirdID.csv')
#Subset to required data
birth_date <- subset(birth_date, select=c('BirdID', 'BirthDate'))

#Data produced by R script 'Identifying_dispersers_and_philos_final_FPID_sub'
disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")


#Check data
str(birth_date)
str(disp_philo)




# Create df of dispersers: containing their birth and disperse dates -------------------------------------

#Subset to dispersers
disp_dates <- disp_philo %>%
  filter(Disperse == 1) %>%
  select(c('BirdID', 'Disperse', 'DispersalDate', 'FieldPeriodID')) #select columns needed to calculate the difference between birth and disperse date


#Add birth date
disp_birth_dates <- merge(disp_dates, birth_date, by='BirdID')


#Add a new column for the age at dispersal, this will be calculated and autofilled later
disp_birth_dates["Age_at_disp"] <- NA


#Change the format of BirthDate to match PeriodStart date, so they can be compared later
disp_birth_dates$BirthDate <- as.Date(disp_birth_dates$BirthDate , "%d/%m/%Y")




# Calculate and plot age at dispersal (in days) ---------------------------

#Calculate difference between birth and dispersal date (in days) and the mean
disp_birth_dates$Age_at_disp <- difftime(disp_birth_dates$DispersalDate, disp_birth_dates$BirthDate, units = c("days"))
mean(disp_birth_dates$Age_at_disp)
range(disp_birth_dates$Age_at_disp)



#Plot data
a <- ggplot(disp_birth_dates, aes(x=Age_at_disp)) 
a <- a + theme_minimal() +
  geom_histogram(binwidth=50,color="black",boundary = 0, closed = "left") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = mean(Age_at_disp)), linetype = "dashed", size = 0.6)
a


#Subset so it can be merged with philopatrics later in the script
age_at_disp <- subset(disp_birth_dates, select=c('BirdID', 'Age_at_disp', 'DispersalDate', 'BirthDate'))




# Create a subset that removes individuals that dispersed at <6 mo --------
adjust_age_at_disp <- age_at_disp %>% filter(!(Age_at_disp < 180))






# Remove philos under mean disp age ---------------------------------------
#Philopatrics that are below the mean age of dispersal may only be philopatrics because they are young and haven't reached the right age of dispersal (in case age is a driver of dispersal), rather than remaining philopatric because they are not exploratory
#Therefore, determine 95%CI and remove any philos that are below lower bound of mean age at dispersal

#Determine 95% CI 
a <- mean(adjust_age_at_disp$Age_at_disp)
s <- sd(adjust_age_at_disp$Age_at_disp)
n <- 90
error <- qnorm(0.975)*s/sqrt(n)
lower <- a - error
upper <- a + error
lower
upper

#therefore, if age of philopatric is below the upper bound, remove from dataset

#retrieve philo data
philo <- disp_philo %>%
  filter(Disperse == 0) 


philo_birth <- merge(philo, birth_date, by = c('BirdID'))


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
date_last_seen <- merge(philo_birth, status_latest, by =c('BirdID'))
date_last_seen$BirthDate <- as.Date(date_last_seen$BirthDate , "%d/%m/%Y")


#calculate age last seen on Cousin
date_last_seen$Age_last_seen <- difftime(date_last_seen$Date_last_seen, date_last_seen$BirthDate, units = c("days"))



#remove individuals that have not reached upper 95% CI
older <- date_last_seen %>% filter(Age_last_seen > 508)

younger <- date_last_seen %>% filter(Age_last_seen < 508)


##Save
#File including ONLY philopatrics that have reached minimum age

#Create column for age at dispersal - keep filled with NA as individuals are philopatric
older$Age_at_disp <- NA

#subset to match columns to dispersal vector, then rbind with dispersal vector
older.simp <- subset(older, select=c('BirdID', 'Age_at_disp','DispersalDate', 'BirthDate'))
older.simp <- rbind(Age_at_dispersal, older.simp)


#Add year of birth and year of dispersal as they are required as fixed and random effects for model
older.simp$DispersalDate <- as.Date(older.simp$DispersalDate, "%Y-%m-%d") 

older.simp$DispersalYear <- as.numeric(format(older.simp$DispersalDate,'%Y'))
older.simp$BirthYear <- as.numeric(format(older.simp$BirthDate,'%Y'))



#Save
write.csv(older.simp, '4_Age_at_disp/Age_at_dispersal_older.csv', row.names = FALSE)


#File including all philopatrics, regardless of age last seen
all <- Date_last_seen
all$Age_at_disp <- NA
all <- subset(all, select=c('BirdID', 'Age_at_disp','DispersalDate', 'BirthDate'))
all <- rbind(Age_at_dispersal, all)

#Add year of birth and year of dispersal as they are required as fixed and random effects for model
all$DispersalDate <- as.Date(all$DispersalDate, "%Y-%m-%d") 

all$DispersalYear <- as.numeric(format(all$DispersalDate,'%Y'))
all$BirthYear <- as.numeric(format(all$BirthDate,'%Y'))

#Save
write.csv(all, '4_Age_at_disp/Age_at_dispersal_all.csv', row.names = FALSE)
