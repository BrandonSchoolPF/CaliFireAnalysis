### Brandon Dora's Density plot of the Fires per County in California. 


### Importing Library 
library(readr)
library(ggmap)
library(ggplot2)
library(tibble)
library(tidyverse)
library(zipcode)
library(remotes)

# setting working directory 
setwd("C:/Users/Brandon Dora/Desktop/DataMining")



#implementing Ben's cleaned dataset
 testFrame3 <- read.csv("California_Fire_Incidents.csv")
  
  testFrame3 <- testFrame3[,-2]
  
  testFrame3 <- testFrame3[,-6]
  
  testFrame3 <- testFrame3[,-9]
  
  testFrame3 <- testFrame3[,-14:-16]
  
  testFrame3 <- testFrame3[,-21]
 
  testFrame3 <- testFrame3[,-22]

  testFrame3 <- testFrame3[,-25]
  
  summary(testFrame3$StructuresEvacuated)

  testFrame3 <- testFrame3[,-27]
  
  summary(testFrame3$StructuresThreatened)
  
  testFrame3 <- testFrame3[,-28:-29]
  
  testFrame3 <- testFrame3 %>% relocate(Started, .before = Extinguished)
  
  testFrame3 <- testFrame3 %>% relocate(Name, .before = AcresBurned)
  
  testFrame3 <- testFrame3 %>% relocate(Longitude, .before = Location)
  
  testFrame3 <- replace_na(testFrame3, list(AirTankers = 0, CrewsInvolved = 0, Dozers = 0, Engines = 0, Fatalities = 0, Helicopters = 0, Injuries = 0, PersonnelInvolved = 0, StructuresDamaged = 0, StructuresDestroyed = 0, StructuresThreatened = 0, WaterTenders = 0))


## creating and cleaning data frames to map out the state of california
  
states <- map_data("state")
California <- subset(states, region == "california")
California$Longitude <- California$long
California$Latitude <- California$lat
California$long = NULL
California$lat = NULL
California$region <- str_to_title(California$region)
counties <- map_data("county")
californiaCounties <- subset(counties, region == "california")
californiaCounties$Counties <- str_to_title(californiaCounties$subregion)
californiaCounties$subregion = NULL
californiaCounties$Longitude <- californiaCounties$long
californiaCounties$Latitude <- californiaCounties$lat
californiaCounties$long = NULL
californiaCounties$lat = NULL

### creating a new dataframe based on our original to focus on the counties, and the number of times a fire has occured in each of them. 


testFrame2 <- table(testFrame3$Counties)
testFrame2

testFrame2 <- as.matrix(testFrame2)
testFrame2 <- as.data.frame(testFrame2)
testFrame2$Counties <- c("Alameda", "Alpine", "Amador", "Butte", "Calaveras", "Colusa", "Contra Costa", "Del Norte", "El Dorado", "Fresno", "Glenn", "Humboldt", "Inyo", "Kern", "Kings", "Lake", "Lassen", "Los Angeles", "Madera", "Marin", "Mariposa", "Mendocino", "Merced", "Mexico", "Modoc", "Mono", "Monterey", "Napa", "Nevada", "Orange", "Placer", "Plumas", "Riverside", "Sacramento", "San Benito", "San Bernardino", "San Diego", "San Joaquin", "San Luis Obispo", "San Mateo", "Santa Barbara", "Santa Clara", "Santa Cruz", "Shasta", "Sierra", "Siskiyou", "Solano", "Sonoma", "Stanislaus", "State of Nevada", "State of Oregon", "Sutter", "Tehama", "Trinity", "Tulare", "Tuolumne", "Ventura", "Yolo", "Yuba")
testFrame2$FireCount <- testFrame2$V1
testFrame2$V1 = NULL



### combining our new test frame with our county dataframe. 
californiaCounties <- left_join(californiaCounties, testFrame2, by="Counties")
californiaCounties[is.na(californiaCounties)] = 0



# creating our state map
caliFire <- ggplot(data = California, mapping = aes(x = Longitude, y = Latitude, group = group))
caliFire <- caliFire + geom_polygon(data = californiaCounties, color = "white") + geom_polygon(color = "black", fill = NA)
caliFire <- caliFire + coord_fixed(1.3)

### the state map sectioned by counties



caliFire


### adding our data and creating our 2d density map on top of a heatmap



caliFire <- caliFire + geom_polygon(data = californiaCounties, aes(fill = FireCount))
caliFire<- caliFire + ggtitle("Fire Count in California by County")
caliFire2d <- caliFire + stat_density2d()
caliFire2d 
