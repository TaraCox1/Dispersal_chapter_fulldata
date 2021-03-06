####Calculate dispersal distance

rm(list = ls())

library(dplyr)
library(magrittr)

setwd("Data_wrangling")


# Retrieve data -----------------------------------------------------------

#First need dispersal data 
#Data produced by R script 'Identifying_dispersers_and_philos_final_FPID_sub'
disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")


#Correct data format
#Including data type
disp_philo$BirdID <- factor(disp_philo $BirdID)
disp_philo$FieldPeriodID <- factor(disp_philo $DisperseFPID) #first field period seen in new territory
disp_philo$TerritoryID <- factor(disp_philo$DisperseTerritoryID)
disp_philo$NatalTerritory <- factor(disp_philo$NatalTerritory)


#Filtering down to required data by removing philopatrics and additional info on dispersers 
disp <- disp_philo [!(disp_philo $Disperse==0),]



# Read in territory location data  ----------------------------------------

t_data <- read.csv('DB_tables_queries/Territory_sizes_2021-01-08.csv')


#As there are multiple methods of measurements for some territories within a season, they must be filtered to only contain the latest method
#Remember: D > G > P method
#Therefore, if we group by TerritoryID and FieldPeriodID, arrange them into size/alphabetical order including method, I then slice to remove the first record within each grouping
#Refine data

t_location  <- t_data %>%
               filter(Island=="CN") %>% #Only Cousin
               select(TerritoryID, FieldPeriodID, Easting, Northing, SizeMethod, TerritorySize) %>% #Required data
               filter(!(is.na(Easting))) %>% #Remove rows without long and lat
               group_by(TerritoryID, FieldPeriodID) %>% 
               arrange(TerritoryID, FieldPeriodID, SizeMethod) %>% 
               slice(1)


t_location_final <- t_location %>% 
                    select(!(SizeMethod)) %>%
                    rename(DisperseTerritorySize = TerritorySize)


# Merge -------------------------------------------------------------------

#Merge location data with personality data, first to obtain dispersal territory location (for the field period bird was seen in their new territory)...
new_terri_loc <- merge(t_location_final, disp, by= c('TerritoryID', 'FieldPeriodID'), all.y = TRUE)


new_terri_loc %<>% rename(DisperseTerritory = TerritoryID,
                          DisperseTerrEasting = Easting,
                          DisperseTerrNorthing = Northing)



#...Then to obtain natal territory location (for the field period bird was seen in their new territory)
t_location_final %<>% rename(NatalTerritory = TerritoryID, 
                             NatalTerritorySize = DisperseTerritorySize)


both_terri_loc <- merge(t_location_final, new_terri_loc, by=c('NatalTerritory', 'FieldPeriodID'), all.y = TRUE)



both_terri_loc %<>% rename(NatalTerrEasting = Easting,
                           NatalTerrNorthing = Northing)





# Repeat for FP prior dispersal -------------------------------------------
#...As well as the location of the dispersal territory the season prior to dispersal
t_location_final %<>%
  rename(LastFPIDNatal = FieldPeriodID,
         NatalTerrEastingPRIOR = Easting,
         NatalTerrNorthingPRIOR = Northing,
         NatalTerritorySizePRIOR = NatalTerritorySize)


natal_terri_loc.PRIOR <- merge(t_location_final, both_terri_loc, by= c('NatalTerritory', 'LastFPIDNatal'), all.y = TRUE)


t_location_final %<>%
  rename(DisperseTerritory = NatalTerritory,
         DisperseTerrEastingPRIOR = NatalTerrEastingPRIOR,
         DisperseTerrNorthingPRIOR = NatalTerrNorthingPRIOR,
         DisperseTerritorySizePRIOR = NatalTerritorySizePRIOR)


both_terri_loc.PRIOR <- merge(t_location_final, natal_terri_loc.PRIOR, by= c('DisperseTerritory', 'LastFPIDNatal'), all.y = TRUE)






# Merge dfs ---------------------------------------------------------------

#both_terri_loc.all <- merge(both_terri_loc, both_terri_loc.PRIOR, by= c('BirdID', 'FieldPeriodID', 'DisperseTerritory', 'NatalTerritory', 'FieldPeriodPreDisperse'), all = TRUE)


terri.location <- both_terri_loc.PRIOR

terri.location$NatalTerrEasting <- 
  ifelse(is.na(terri.location$NatalTerrEasting), terri.location$NatalTerrEastingPRIOR, terri.location$NatalTerrEasting)

terri.location$NatalTerrNorthing <- 
  ifelse(is.na(terri.location$NatalTerrNorthing), terri.location$NatalTerrNorthingPRIOR, terri.location$NatalTerrNorthing)

terri.location$NatalTerritorySize <- 
  ifelse(is.na(terri.location$NatalTerritorySize), terri.location$NatalTerritorySizePRIOR, terri.location$NatalTerritorySize)


terri.location$DisperseTerrEasting <- 
  ifelse(is.na(terri.location$DisperseTerrEasting), terri.location$DisperseTerrEastingPRIOR, terri.location$DisperseTerrEasting)

terri.location$DisperseTerrNorthing <- 
  ifelse(is.na(terri.location$DisperseTerrNorthing), terri.location$DisperseTerrNorthingPRIOR, terri.location$DisperseTerrNorthing)

terri.location$DisperseTerritorySize <- 
  ifelse(is.na(terri.location$DisperseTerritorySize), terri.location$DisperseTerritorySizePRIOR, terri.location$DisperseTerritorySize)


# Calculate distance between territories ----------------------------------

#Use pythag to calculate distance between natal and dispersal territory

#easting=lat/x
#northing=long/y
#distance = sqr root of (lat of disperse location - lat of natal location)squared + (long of disperse location - long of natal location)squared

disperse.dist <- terri.location

disperse.dist$Distance <- sqrt((disperse.dist$DisperseTerrNorthing - disperse.dist$NatalTerrNorthing)^2 + (disperse.dist$DisperseTerrEasting - disperse.dist$NatalTerrEasting)^2)







# Check distance when using east and north for new territory FP --------

#Get field period first seen in new territory





#Add easting and northing for natal and new territories








# Double check, plot & save -----------------------------------------------

#Check whether any territories are missing distance calculations 
which(is.na(disperse.dist$Distance))


#Plot
hist(disperse.dist$Distance)


#Save
final <- subset(disperse.dist, select = c('BirdID', 'Distance', 'NatalTerritorySize', 'DisperseTerritorySize'))

write.csv(final, '9_Dispersal_distance/Dispersal_distance.csv', row.names = FALSE)


