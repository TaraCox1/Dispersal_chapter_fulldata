##Find group size of natal territory the season prior to dispersal


# Load packages -----------------------------------------------------------

rm(list = ls())

getwd()
setwd("Data_wrangling")

library(tidyverse)
library(magrittr)



# Determine group sizes for every season + territory ----------------------


#Read in group size data, check and correct any variables
breed_groups <- read.csv('DB_tables_queries/sys_BreedGroupLocation+Status.csv')
str(breed_groups)

breed_groups$BirdID <- factor(breed_groups$BirdID)
breed_groups$FieldPeriodID <- factor(breed_groups$FieldPeriodID)
breed_groups$TerritoryID <- factor(breed_groups$TerritoryID)


#Remove vague status' (SEEN1 etc.)
#Note: I have not removed XF birds as XF indicates a chick was moved INTO the territory from another (rather than out of the territory)
bg_core <- breed_groups %>%
  filter(!(Status == "SEEN1" | Status == "SEEN2" | Status == "U" |
           Status == "NS" | Status == "EGG" | Status == "FLOAT" |
          Status == "BrU" | Status == "NSA"))


#Check NSAs - if bird has >1 NSA status within a FPID, then remove both rows. I
bg_NSA <- breed_groups %>% 
              filter(Status == "NSA") %>% 
              filter(!(TerritoryID == -3)) %>%
              group_by(BirdID, FieldPeriodID) %>%
              filter(!(n()>1)) #remove birds that have >1 row within the same field period i.e. >1 NSA status within a FP
  

#Rbind NSAs with core status list 
bg_all <- rbind(breed_groups, bg_NSA)



#Find group size corresponding to field period prior dispersal by counting the number of entries per group per FPID
bg_count <- bg_all %>%
        group_by(FieldPeriodID, TerritoryID) %>%
        summarise(GroupSize.all = n()) 


#Find group size WITHOUT individuals <6months i.e. CH, FL & OFL
bg_countold <- bg_all %>%
          filter(!(Status == "CH" | Status == "FL" | Status == "OFL" | Status == "XF")) %>% 
          group_by(FieldPeriodID, TerritoryID) %>%
          summarise(GroupSize.old = n()) 









# Determine group size FPID prior to dispersal ----------------------------

#Data produced by R script 'Identifying_dispersers_and_philos_final_FPID_sub'
disp_philo <- read.csv("2_Identifying_dispersers_and_philopatrics/disp_and_philo.csv")



#Change name from FieldPeriodID in df BreedGroupsCount/Old to LastFPIDNatal so it can merge with dispersal + philo data
#Rename TerritoryID to NatalTerritory, as we want to know the group size of the natal territory 
bg_count %<>% rename(LastFPIDNatal = FieldPeriodID, NatalTerritory = TerritoryID)
bg_countold %<>% rename(LastFPIDNatal = FieldPeriodID, NatalTerritory = TerritoryID)



#Merge with dispersal/philo data for:
#Group size with all individuals
disp_philo_count <- merge(disp_philo, bg_count, by=c("LastFPIDNatal", "NatalTerritory"),all.x=TRUE)

#Group size with only older (>6month) individuals
disp_philo_countold <- merge(disp_philo_count, bg_countold, by=c("LastFPIDNatal", "NatalTerritory"),all.x=TRUE)




#Make manual amendments
#FPID174 (winter 2020) was brief, so there was not enough time to do full island census
#Therefore, group size for birds 7132 and 6884 are only 1
#7132 does not have a group size the FPID prior to dispersal, so cannot make an amendment. Individual is too young to be included in analyses (will be subsetted out later), so have left error
#Group size during FPID173 for 6884 is available, so have manually changed to blank so it will be calculated in the script below
disp_philo_countold[disp_philo_countold$BirdID==6884, "GroupSize.old"] <- NA
disp_philo_countold[disp_philo_countold$BirdID==6884, "GroupSize.all"] <- NA




#Determine group size for FPID 2 seasons prior dispersal ---------------------------

#Repeat the above
#Change name from FieldPeriodID in df BreedGroupsCount/Old to SecondLastFPIDNatal so it can merge with personality data
bg_count %<>% rename(SecondLastFPIDNatal = LastFPIDNatal, GroupSize.all.2 = GroupSize.all)
bg_countold %<>% rename(SecondLastFPIDNatal = LastFPIDNatal, GroupSize.old.2 = GroupSize.old)


#Merge with dispersal/philo data for:
#Group size with all individuals
disp_philo_count_2 <- merge(disp_philo_countold, bg_count, by=c("SecondLastFPIDNatal", "NatalTerritory"), all.x=TRUE)

#Group size with only older (>6month) individuals
disp_philo_countold_2 <- merge(disp_philo_count_2, bg_countold, by=c("SecondLastFPIDNatal", "NatalTerritory"), all.x=TRUE)



# If group size for FPID prior dispersal = NA, use FPID 2 seasons  --------
disp_philo_countold_2$GroupSize.all <- 
  ifelse(is.na(disp_philo_countold_2$GroupSize.all), disp_philo_countold_2$GroupSize.all.2, disp_philo_countold_2$GroupSize.all)
disp_philo_countold_2$GroupSize.old <- 
  ifelse(is.na(disp_philo_countold_2$GroupSize.old), disp_philo_countold_2$GroupSize.old.2, disp_philo_countold_2$GroupSize.old)


#Remove group size two seasons prior to dispersal data
disp_philo_tidy <- disp_philo_countold_2 %<>%
  select(-GroupSize.all.2, -GroupSize.old.2)






# Manual edits ------------------------------------------------------------


group_size <- subset(disp_philo_tidy, select=c('BirdID', 'GroupSize.all', 'GroupSize.old'))


#Amend entry 7132 to NA as their group size is 1 (and was 0 the season prior)
group_size[group_size$BirdID==7132, "GroupSize.old"] <- NA
group_size[group_size$BirdID==7132, "GroupSize.all"] <- NA


#Manually change group size for 5596 from 1 to 3
#This is because the FPID prior to gaining dominance (bird is philopatric), the focal bird, BRF and BRM were NSA. 
#However, all 3 were seen the season the focal bird gained dominance (as a TBRF), meaning all 3 individuals would have been resident on the territory during FP prior dispersal
group_size[group_size$BirdID==5596, "GroupSize.all"] <- 4
group_size[group_size$BirdID==5596, "GroupSize.old"] <- 4


#Some territories only have a ALL group size of 2, but not including the focal individual. 
#This is because we know the individual was resident in the territory as the pedigree states it is their natal territory.
#However, because they were never observed in the territory, they are not included in the group size calculations (as they are based off of the status list)
#Is there a way to check whether the focal individual is included in the group size list without doing it manually?

#Manual edit for now (note: these birds are not added to group size OLD because they were all CH-OFL status)
group_size <- group_size %>% 
  mutate(GroupSize.all=ifelse(BirdID==6014 | BirdID==5573 | BirdID==3453 | BirdID==1729 | BirdID==1821 | BirdID==5605 | 
                                BirdID==5842 | BirdID==6030 | BirdID==6029 | BirdID==6196 | BirdID==6546 | BirdID==6313 |
                                BirdID==6564,3,GroupSize.all))



# Plot data ---------------------------------------------------------------




ggplot(group_size, aes(x=GroupSize.all)) + 
  geom_histogram(breaks=seq(1, 6, by=1), 
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


ggplot(group_size, aes(x=GroupSize.old)) + 
  geom_histogram(breaks=seq(1, 6, by=1), 
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






# Save --------------------------------------------------------------------

write.csv(group_size, '6_Group_size/Group_size.csv', row.names = FALSE)


