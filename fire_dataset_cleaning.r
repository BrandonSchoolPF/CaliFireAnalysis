# Script written by: Benjamin Garcia

library(readr)
fire_dataset_Cleaning <- read_csv("fire_dataset - Cleaning.csv")
View(fire_dataset_Cleaning)

#summary(fire_dataset_Cleaning)
#str(fire_dataset_Cleaning)

#all rows in active column are False because none of them are active anymore. This information is not particularly useful
#so I will remove the column below.
fire_dataset_Cleaning <- fire_dataset_Cleaning[,-2]

# The canonical url column provides the ends of a url, but no mention of the website being references. There is not any
# real data analysis value in keeping these so I will remove the column below.
fire_dataset_Cleaning <- fire_dataset_Cleaning[,-6]

# County id is useful in that it can help locate the fire. However, since lat and long along with county name are already
#present I feel there is no reason to keep these values. I will remove the column below.
fire_dataset_Cleaning <- fire_dataset_Cleaning[,-9]

# The column featured is mostly false and the column final is mostly true. Besides being unable to understand what the column
# is trying to say the columns it provides no unique data that helps understand differences between the fires. Additionally,
# fuel type column is mostly null and seems to provide very little insight about the fires we are trying to understand.
# I will remove all three columns together below.
fire_dataset_Cleaning <- fire_dataset_Cleaning[,-14:-16]

#The percent contained column is all 100's because all fires reported have been contained. This provides no real insight
# and I will remove the column below.
fire_dataset_Cleaning <- fire_dataset_Cleaning[,-21]

# The public data has all true values since all the fires were public. This provides very little insight and I will
#remove the column below.
fire_dataset_Cleaning <- fire_dataset_Cleaning[,-22]

# The status column is contains all finalized observations since all fires reported have been finalized. This
# does not provide useful information and I will remove the column below.
fire_dataset_Cleaning <- fire_dataset_Cleaning[,-25]


summary(fire_dataset_Cleaning$StructuresEvacuated)
#All of the values in the structuresEvacuated column are NA. I will remove the column below.
fire_dataset_Cleaning <- fire_dataset_Cleaning[,-27]

summary(fire_dataset_Cleaning$StructuresThreatened)

# In terms of data analysis Uniqueid column provides no useful insight. Additionally, the dates in the update
# column does not provide good insight since we already have start and extingushed dates and update times reveal nothing
# about fire duration. I will remove both uniqueid and update columns together below.
fire_dataset_Cleaning <- fire_dataset_Cleaning[,-28:-29]



library(dplyr)
#I moved the started date and time to before extinguished for clarity. I will do this below.
fire_dataset_Cleaning <- fire_dataset_Cleaning %>% relocate(Started, .before = Extinguished)

#I moved the names of the fires to the front of the data set for clarity. I do this below.
fire_dataset_Cleaning <- fire_dataset_Cleaning %>% relocate(Name, .before = AcresBurned)

#I moved Longitude right after Latitude for clarity. I do this below.
fire_dataset_Cleaning <- fire_dataset_Cleaning %>% relocate(Longitude, .before = Location)

library(tidyr)
#There is no way to know for sure what the NA values truly are for these columns. I have decided to make the NA's
# 0 since they are not reported and other values that are not zero are. Please consider the columns with columns
#with care.


summary(fire_dataset_Cleaning$Started)
#fire_dataset_Cleaning <- replace_na(fire_dataset_Cleaning, list(AirTankers = 0, CrewsInvolved = 0, Dozers = 0, Engines = 0, Fatalities = 0, Helicopters = 0, Injuries = 0, PersonnelInvolved = 0, StructuresDamaged = 0, StructuresDestroyed = 0, StructuresThreatened = 0, WaterTenders = 0))

min(fire_dataset_Cleaning$Started)
min(fire_dataset_Cleaning$Extinguished)
summary(fire_dataset_Cleaning$Extinguished)
#59 NA's for extinguished times

#hi