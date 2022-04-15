# Script written by Alejandro Cepeda

library(shiny)
library(shinythemes)
library(tidyverse)
library(rsconnect)

# Benjamin's data cleaning script as a function
cleanFireData <- function() {
  setwd("C:/Users/alece/My Drive/University/USF/SPRING 22/LIS 4761 - Data & Text Mining/Final Project/introdatafinalproject")

  testFrame <- read.csv("California_Fire_Incidents.csv")
  
  testFrame <- testFrame[,-2]
  
  testFrame <- testFrame[,-6]
  
  testFrame <- testFrame[,-9]
  
  testFrame <- testFrame[,-14:-16]
  
  testFrame <- testFrame[,-21]
 
  testFrame <- testFrame[,-22]

  testFrame <- testFrame[,-25]
  
  summary(testFrame$StructuresEvacuated)

  testFrame <- testFrame[,-27]
  
  summary(testFrame$StructuresThreatened)
  
  testFrame <- testFrame[,-28:-29]
  
  testFrame <- testFrame %>% relocate(Started, .before = Extinguished)
  
  testFrame <- testFrame %>% relocate(Name, .before = AcresBurned)
  
  testFrame <- testFrame %>% relocate(Longitude, .before = Location)
  
  testFrame <- replace_na(testFrame, list(AirTankers = 0, CrewsInvolved = 0, Dozers = 0, Engines = 0, Fatalities = 0, Helicopters = 0, Injuries = 0, PersonnelInvolved = 0, StructuresDamaged = 0, StructuresDestroyed = 0, StructuresThreatened = 0, WaterTenders = 0))
  
  return(testFrame)
}

# Define UI
ui <- fluidPage(
  # I plan on adding a theme to the app,
  # but no theme is avaliable when running
  #shinythemes::themeSelector(),
  #theme = shinytheme("darkly"),
  titlePanel("California Forest Fire Map"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var1", "Variable:",
                  c("Acres Burned" = "AcresBurned",
                    "Archive Year" = "ArchiveYear",
                    "# of Fatalities" = "Fatalities",
                    "# Injured" = "Injuries",
                    "# of Structures Damaged" = "StructuresDamaged",
                    "# of Structures Destroyed" = "StructuresDestroyed")),
      
      selectInput("var2", "Variable:",
                  c("Acres Burned" = "AcresBurned",
                    "Archive Year" = "ArchiveYear",
                    "# of Fatalities" = "Fatalities",
                    "# Injured" = "Injuries",
                    "# of Structures Damaged" = "StructuresDamaged",
                    "# of Structures Destroyed" = "StructuresDestroyed")),
      
      selectInput("var2", "Variable:",
                  c("Acres Burned" = "AcresBurned",
                    "Archive Year" = "ArchiveYear",
                    "# of Fatalities" = "Fatalities",
                    "# Injured" = "Injuries",
                    "# of Structures Damaged" = "StructuresDamaged",
                    "# of Structures Destroyed" = "StructuresDestroyed"))
      ),
    
    mainPanel(
      plotOutput("scatterplot"),
      plotOutput("mapplot")
    )
  )
)

# server logic containing data prep and plotting for the app
server <- function(input, output) {
  # run Ben's data cleaning script and only keep wanted columns
  fire_df <- cleanFireData() %>%
    select(AcresBurned, ArchiveYear, Counties, Fatalities, Injuries, 
           StructuresDamaged, StructuresDestroyed, Latitude, Longitude) %>%
    replace(is.na(.), 0) %>%
    mutate_at("Counties", tolower) %>%
    mutate_at(c("ArchiveYear","Fatalities","Injuries","StructuresDamaged","StructuresDestroyed"), as.numeric)
  
  # scatterplot showing the desired input y-axis 
  output$scatterplot <- renderPlot(
    ggplot(fire_df, aes(x=fire_df[,input$var1], y=fire_df[,input$var2])) +
      geom_point()
  )
  
  # generate us map data of all counties
  us <- map_data("county") %>%
    filter(region=="california")
  
  # ***(testing phase)***
  # map showing the desired variable on the California state map
  output$mapplot <- renderPlot(
    ggplot(fire_df, aes(map_id = Counties)) +
      geom_map(map = us, aes(fill = fire_df[,input$var1])) +
      expand_limits(x = us$long, y = us$lat) +
      coord_map() + guides(fill=guide_legend((title=input$var1)))
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)