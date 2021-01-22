#I don't know which packages you need for this, so here's all of them that I use in my analyses (sorry)
#Load packages to the library 
library(tidyverse)


### Datasets needed (I'll send these)
TerritoryQuality_calc <- read.csv("DB_tables_queries/sys_TerritoryQuality.csv")
FPS_SummerIndex <- read.csv("DB_tables_queries/AllFPS_SummerIndex.csv")


# Territory quality: fixing missing datapoints ----------------------------

#Is it a summer or winter season? (Calculate for all FPs whether offspring were born or not), then calculate midpoint (used to determine chronology)
FPS_SummerIndex$Season <- NA
FPS_SummerIndex$Season[FPS_SummerIndex$SummerIndex>=0.5] <- "Summer"
FPS_SummerIndex$Season[FPS_SummerIndex$SummerIndex<0.5] <- "Winter"
FPS_SummerIndex %<>% 
  mutate(PeriodStart = as.Date(PeriodStart, format = "%d/%m/%Y"),
         PeriodEnd = as.Date(PeriodEnd, format = "%d/%m/%Y"))


FPS_SummerIndex$midpoint <- as.Date((FPS_SummerIndex$PeriodStart) + 
                                      ((difftime(FPS_SummerIndex$PeriodEnd, FPS_SummerIndex$PeriodStart , units = c("days")))/2))

FPS_SummerIndex <- subset(FPS_SummerIndex, select = c("TerritoryID", "FieldPeriodID", "Season", "midpoint"))

#Bind information about TQ for all FP where data is available
TerritoryQuality_calc <- unique(subset(TerritoryQuality_calc, select = c("TerritoryID", "FieldPeriodID", "TQcorrected"), Island=="CN"))

#Link the dataset we have territory quality info for to the complete list of FPs and territories (even if there's no TQ data)
TerritoryQuality_calc <- unique(merge(TerritoryQuality_calc, FPS_SummerIndex, by.x=c("TerritoryID", "FieldPeriodID"),
                                      by.y=c("TerritoryID", "FieldPeriodID"), 
                                      all.x = TRUE,
                                      all.y = TRUE))

#Arrange 
TerritoryQuality_calc <- TerritoryQuality_calc %>%
  arrange(TerritoryID, midpoint) %>%
  group_by(TerritoryID) 

colnames(TerritoryQuality_calc)[colnames(TerritoryQuality_calc) == 'TQcorrected'] <- 'TQ'
TerritoryQuality_calc$prevTQ <- TerritoryQuality_calc$TQ
TerritoryQuality_calc$nextTQ <- TerritoryQuality_calc$TQ

#Break down into summer and winter FPs as we will need to calculate the estimated TQ based on matching seasons 
TQ_winter <- subset(TerritoryQuality_calc, Season=="Winter")
TQ_summer <- subset(TerritoryQuality_calc, Season=="Summer")

#####Sara's code 
#If there is an NA for a particular FP, work out the mean between prevTQ and nextTQ, and then replace any NAs in the TQ column with the meanTQ

#Print the value of the most recent field period before current NA
TQ_winter <- TQ_winter %>% 
  group_by(TerritoryID) %>% 
  tidyr::fill(prevTQ, .direction='updown')

#Print the value of the most recent field period after current NA
TQ_winter <- TQ_winter %>% 
  group_by(TerritoryID) %>% 
  tidyr::fill(nextTQ, .direction='downup')

#Calculate mean of prev and next TQs 
TQ_winter$TQ <- (TQ_winter$prevTQ + TQ_winter$nextTQ)/2

#Repeat for summer seasons

#Print the value of the most recent field period before current NA
TQ_summer <- TQ_summer %>% 
  group_by(TerritoryID) %>% 
  tidyr::fill(prevTQ, .direction='updown')

#Print the value of the most recent field period after current NA
TQ_summer <- TQ_summer %>% 
  group_by(TerritoryID) %>% 
  tidyr::fill(nextTQ, .direction='downup')

#Calculate mean of prev and next TQs 
TQ_summer$TQ <- (TQ_summer$prevTQ + TQ_summer$nextTQ)/2

#Bind summer and winter seasons back together
TQ <- rbind(TQ_summer, TQ_winter)

#Refine TQ dataset 
TQ <- subset(TQ, select = c("TerritoryID", "FieldPeriodID", "TQ"))


#Save
write.csv(TQ, "DB_tables_queries/TQEllieSara.csv", row.names = FALSE)
