####Insect abundance

#Third trial of calculating abundance

#Script looks at insect abundance from the perspective of sum_X_veg, as per Frank's paper

#Combination of techniques from Insect & Insect_2
#Where data is collated insect abundance (similar to Insect_2)
#But data is taken from TerritoryQuality query (similar to Insect_1)
#Using sum_X_veg column


#This file:

#a) Adds up all surveys each year (incl repeats for the same territory), then calculates a mean

#b) Adds up the mean of each territory within a year


rm(list = ls())

library(tidyverse)
library(magrittr)

getwd()
setwd("Data_wrangling")


# 1. Prep data ------------------------------------------------------------


#Retrieve TQ
tq <- read.csv('DB_tables_queries/sys_TerritoryQuality.csv')

#Filter to CN
tq <- tq[(tq$Island=="CN"),]


#Correct format
tq$Year <- as.numeric(tq$Year)


#Read in FPID to access dates
fpid <- read.csv('DB_tables_queries/tblFieldPeriodIDs.csv')

fpid <- fpid[(fpid$Island=="CN"),]

fpid <- subset(fpid, select=c('FieldPeriodID', 'PeriodStart', 'PeriodEnd'))


#Merge with TQ data
tqfp <- merge(tq, fpid, by=c('FieldPeriodID'))

#Correct format
tqfp$PeriodStart <- as.Date(tqfp$PeriodStart, '%d/%m/%Y')

#Include only month
tqfp$StartMonth <- format(tqfp$PeriodStart, "%m")

#Remove the 0 prior to month e.g. 07 to 7
tqfp$StartMonth <- gsub("(?<![0-9])0+", "", tqfp$StartMonth, perl = TRUE)

#Change month from numeric to string
tqfp$StartMonth <- as.numeric(tqfp$StartMonth)
tqfp$StartMonth <- month.abb[tqfp$StartMonth]

#Add column for whether FPID was winter or summer
tqfp$StartMonth2 <- ifelse(tqfp$StartMonth == "Jan", "W",
                           ifelse(tqfp$StartMonth == "Feb", "W",
                                  ifelse(tqfp$StartMonth == "Nov", "W", 
                                         ifelse(tqfp$StartMonth == "Dec", "W","S"))))


#Save winter entries in separate vector
winter <- tqfp[(tqfp$StartMonth2=="W"),]

#Remove winter seasons
tqfp <- tqfp[(tqfp$StartMonth2=="S"),]


#remove rows with NA in TQcorrected
tqfp <- tqfp[!is.na(tqfp$TQcorrected),]


#Correct Year format
tqfp$Year <- as.factor(tqfp$Year)




# 2. Add all surveys together within a year --------

#Group by fieldperiodid then add up all sum_X_veg values together within that year and place the result value in another column
#Note - there is only one sum_X_veg value per territory within a season
(tqfp.1 <- aggregate(tqfp$SumVeg_X_Inverts, by=list(Category=tqfp$Year), FUN=sum))


ggplot(tqfp, aes(x=Year, y=SumVeg_X_Inverts)) + geom_point()

tqfp.1 <- tqfp.1 %>%
  dplyr::rename(LastFPIDNatalYear = Category, InsectAbundance = x)


# 3. Calculate mean of all values within a year ---------------------------
#As there is variation between the number of territories surveyed each season, calculate a mean

(tqfp.2 <- Rmisc::summarySE(tqfp, measurevar="SumVeg_X_Inverts", groupvars="Year"))

tqfp.2 <- tqfp.2 %>%
  dplyr::rename(DispersalYear = Year, Insect.abundance.mean = SumVeg_X_Inverts)

ggplot(tqfp.2, aes(DispersalYear, Insect.abundance.mean)) + 
  geom_point() +  
  geom_errorbar(aes(ymin=Insect.abundance.mean-se, ymax=Insect.abundance.mean+se), width=.1) +
  ylab("mean SumVeg_X_Inverts/Insect.abundance.mean") + 
  xlab("")





# 3. Add this to personality data -----------------------------------------

#First need dispersal data 
disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")


#Correct format
disp_philo$LastFPIDNatalPeriodStart <- as.Date(disp_philo$LastFPIDNatalPeriodStart, '%Y-%m-%d')

#Include only year
disp_philo$LastFPIDNatalYear <- as.numeric(format(disp_philo$LastFPIDNatalPeriodStart, "%Y"))

#Subset to required data
disp_philo.simp <- subset(disp_philo, select=c('BirdID', 'LastFPIDNatalYear'))

#Left join with insect abundance data
insect.abundance <- merge(disp_philo.simp, tqfp.1, by=c('LastFPIDNatalYear'), all.x = TRUE)

#Summer 2020 insect data not collected due to covid
#Use summer 2019 insect data instead
insect.abundance$InsectAbundance <- ifelse(insect.abundance$LastFPIDNatalYear == 2020, 1101.6218, insect.abundance$InsectAbundance)
#Summer 2005 also missing, use summer 2004
insect.abundance$InsectAbundance <- ifelse(insect.abundance$LastFPIDNatalYear == 2005, 757.7015, insect.abundance$InsectAbundance)


# 4. Save -----------------------------------------------------------------

insect.abundance %<>% select(-(LastFPIDNatalYear))

write.csv(insect.abundance, "10_Insect_abundance/Insect_abundance.csv", row.names = FALSE)
