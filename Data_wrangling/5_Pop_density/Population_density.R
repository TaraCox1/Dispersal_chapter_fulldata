##tlc 17/03/2020
##Determine whole island population density for the season an individual was last seen in their natal territory

rm(list = ls())

getwd()
setwd("Data_wrangling")


library(tidyverse)
library(magrittr)
library(plyr)

# Retrieve data  ----------------------------------------------------------

#Status of every individual for each season they've been observed on CN
#Data produced by query sys_BreedGroupLocation+Status
IslandPop <- read.csv('DB_tables_queries/sys_BreedGroupLocation+Status.csv')


#Check and correct data
str(IslandPop)

IslandPop <- subset(IslandPop, select = -c(Island))
IslandPop$FieldPeriodID <- factor(IslandPop$FieldPeriodID)
IslandPop$TerritoryID <- factor(IslandPop$TerritoryID)
IslandPop$BirdID <- factor(IslandPop$BirdID)


#Remove vague statuses 
IslandPop <- IslandPop[-grep("U", IslandPop$Status),]
IslandPop <- IslandPop[-grep("NS", IslandPop$Status),]
IslandPop <- IslandPop[-grep("EGG", IslandPop$Status),]
IslandPop <- IslandPop[-grep("XF", IslandPop$Status),]


#Create new vector for finding population size excluding individuals <6 months old
IslandPopOld <- IslandPop
IslandPopOld <- IslandPopOld[-grep("FL", IslandPopOld$Status),]
IslandPopOld <- IslandPopOld[-grep("CH", IslandPopOld$Status),]


#Some individuals have >1 status per season
#Subset data by removing all columns apart from BirdID and FPID, then subset to only keep unique combinations of FPID and BirdID
#This will produce a list of every individual observed each season
IslandPopUnique <- subset(IslandPop, select = c('FieldPeriodID', 'BirdID'))
IslandPopUnique <- IslandPopUnique %>% distinct()


#Repeat for df of older island residents
IslandPopOldUnique <- subset(IslandPopOld, select = c('FieldPeriodID', 'BirdID'))
IslandPopOldUnique <- IslandPopOldUnique %>% unique()




# Filter to determine pop size --------------------------------------------

#Determine how many individuals have a status for each FPID

#Do this by calculating the sum of entries/rows for each FPID
FPIDPop <- ddply(IslandPopUnique, .(IslandPopUnique$FieldPeriodID), nrow)
names(FPIDPop) <- c("FieldPeriodID", "IslandPopulation")


#Repeat for df of older island residents
FPIDPopOld <- ddply(IslandPopOldUnique, .(IslandPopOldUnique$FieldPeriodID), nrow)
names(FPIDPopOld) <- c("FieldPeriodID", "IslandPopulation")



# Merge pop density data with dispersal and philopatric data --------------------------
#Population density during the season an individual was last seen in their natal terr

#First need dispersal data 
#Data produced by R script 'Identifying_dispersers_and_philos_final_FPID_sub'
disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")


#Change name from FieldPeriodID in df FPIDPop and FPIDPopOld to LastFPIDNatal so it can merge with personality data
FPIDPop %<>% dplyr::rename(LastFPIDNatal = FieldPeriodID)
FPIDPopOld %<>% dplyr::rename(LastFPIDNatal = FieldPeriodID)


#Merge population density and dispersal data
predisp_pop <- merge(disp_philo, FPIDPop,by=c("LastFPIDNatal"), all.x=TRUE)
predisp_pop %<>%  dplyr::rename(PopDensity.all = IslandPopulation)

predisp_pop_old <- merge(predisp_pop, FPIDPopOld,by=c("LastFPIDNatal"),all.x=TRUE)
predisp_pop_old <- predisp_pop_old %>% dplyr::rename(PopDensity.old = IslandPopulation)



# Save --------------------------------------------------------------------

pop_density <- subset(predisp_pop_old, select = c(BirdID, PopDensity.all, PopDensity.old))

write.csv(pop_density, '5_Pop_density/Pop_density.csv', row.names = FALSE)





# Plot --------------------------------------------------------------------


ggplot(pop_density, aes(x=PopDensity.all)) + 
  geom_histogram(breaks=seq(230, 400, by=30), 
                 col="black", fill="palegreen4") +
  theme_classic() +
  scale_y_continuous(name = "Count") +
  theme(axis.ticks = element_line(size=1.5)) +
  theme(axis.ticks.length = unit(7, "pt")) +
  theme(axis.title.x=element_text(face="bold",colour="grey6", size=29, margin = margin(t = 20, r = 0, b = 0, l = 0))) + 
  theme(axis.title.y=element_text(face="bold",colour="grey6", size=29, margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.line=element_line(colour="grey6", size = 1.5)) +
  theme(text = element_text(size=27, colour="black")) +
  theme(axis.text.x = element_text(face="bold", color="grey6", size=17),
        axis.text.y = element_text(face="bold", color="grey6", size=17))


ggplot(pop_density, aes(x=PopDensity.old)) + 
  geom_histogram(breaks=seq(230, 400, by=30), 
                 col="black", fill="palegreen4") +
  theme_classic() +
  scale_y_continuous(name = "Count") +
  theme(axis.ticks = element_line(size=1.5)) +
  theme(axis.ticks.length = unit(7, "pt")) +
  theme(axis.title.x=element_text(face="bold",colour="grey6", size=29, margin = margin(t = 20, r = 0, b = 0, l = 0))) + 
  theme(axis.title.y=element_text(face="bold",colour="grey6", size=29, margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.line=element_line(colour="grey6", size = 1.5)) +
  theme(text = element_text(size=27, colour="black")) +
  theme(axis.text.x = element_text(face="bold", color="grey6", size=17),
        axis.text.y = element_text(face="bold", color="grey6", size=17))

