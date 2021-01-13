
rm(list = ls())

library(dplyr)
library(plyr)
library(ggplot2)

getwd()
setwd("Data_wrangling")

# Read in data ------------------------------------------------------------

#l.ins <- read.csv("DB_tables_queries/Insects.Lewis.csv")
db.ins <- read.csv("DB_tables_queries/qryHD_LoadLight_sqd_InsectsPerYear2.csv")



# Determine whether FP is summer or winter --------------------------------

#Read in FPID to access dates
fpid <- read.csv('DB_tables_queries/tblFieldPeriodIDs.csv')

fpid <- fpid[(fpid$Island=="CN"),]

fpid <- subset(fpid, select=c('FieldPeriodID', 'PeriodStart', 'PeriodEnd'))




# Calculations --------------------------------------------------------

#Correct format
db.ins$Year <- as.numeric(db.ins$Year)


#Merge with TQ data
db.ins.fp <- merge(db.ins, fpid, by=c('FieldPeriodID'))

#Correct format
db.ins.fp$PeriodStart <- as.Date(db.ins.fp$PeriodStart, '%d/%m/%Y')

#Include only month
db.ins.fp$StartMonth <- format(db.ins.fp$PeriodStart, "%m")

#Remove the 0 prior to month e.g. 07 to 7
db.ins.fp$StartMonth <- gsub("(?<![0-9])0+", "", db.ins.fp$StartMonth, perl = TRUE)

#Change month from numeric to string
db.ins.fp$StartMonth <- as.numeric(db.ins.fp$StartMonth)
db.ins.fp$StartMonth <- month.abb[db.ins.fp$StartMonth]

#Add column for whether FPID was winter or summer
db.ins.fp$StartMonth2 <- ifelse(db.ins.fp$StartMonth == "Jan", "W",
                                ifelse(db.ins.fp$StartMonth == "Feb", "W",
                                       ifelse(db.ins.fp$StartMonth == "Nov", "W", 
                                              ifelse(db.ins.fp$StartMonth == "Dec", "W","S"))))


#Save winter entries in separate vector
winter.db <- db.ins.fp[(db.ins.fp$StartMonth2=="W"),]

#Remove winter seasons
db.ins.fp <- db.ins.fp[(db.ins.fp$StartMonth2=="S"),]


#Correct Year format
db.ins.fp$Year <- as.factor(db.ins.fp$Year)



# Missing insect data ------------------------------------------------------------


#Add winter 2020 as summer 2020 fieldwork did not happen due to covid
winter.2020 <- winter.db[(winter.db$Year == 2020),]
winter.2020$Year <- as.factor(winter.2020$Year)
db.ins.fp <- rbind.data.frame(db.ins.fp, winter.2020)

insect <- db.ins.fp
insect <- rename(insect, c("InsectDensity" = "MeanInsects2"))
insect <- rename(insect, c("LastFPIDNatalYear" = "Year"))
insect <- insect[c(3,4)]


#Data are also missing for winter+summer 2001 and 2002
#There are no data for summer 2000 that could be used in place of the missing data for summer 2001
#There was also no survey during summer 2003 (to replace summer 2002 data)
#Will have to leave NAs


# Merge with personality data ---------------------------------------------


#First need dispersal data 
#Data produced by R script 'Identifying_dispersers_and_philos_final_FPID_sub'
disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")


#Correct format
disp_philo$LastFPIDNatalPeriodStart <- as.Date(disp_philo$LastFPIDNatalPeriodStart, "%Y-%m-%d")

#Include only month
disp_philo$LastFPIDNatalYear <- as.numeric(format(disp_philo$LastFPIDNatalPeriodStart, "%Y"))

#Subset to required data
disp_philo.simp <- subset(disp_philo, select=c('BirdID', 'LastFPIDNatalYear'))

#Left join with insect abundance data
insect.density <- merge(disp_philo.simp, insect, by=c('LastFPIDNatalYear'), all.x = TRUE)






write.csv(insect.density, "Insect_density.csv", row.names = FALSE)
