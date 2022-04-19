# Script written by Alejandro Cepeda

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(rsconnect)

# Benjamin's data cleaning script as a function
cleanFireData <- function() {
  # working directories
  setwd("C:/Users/alece/My Drive/University/USF/SPRING 22/LIS 4761 - Data & Text Mining/Final Project/introdatafinalproject")
  #setwd("~/Google Drive/University/USF/SPRING 22/LIS 4761 - Data & Text Mining/Final Project/introdatafinalproject")
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
  #theme = shinytheme("darkly"),
  titlePanel("California Forest Fire Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Scatter Plot X-axis:",
                  c("Acres Burned" = "AcresBurned",
                    "Month" = "Month",
                    "Year" = "Year",
                    "# of Fatalities" = "Fatalities",
                    "# Injured" = "Injuries",
                    "# of Structures Damaged" = "StructuresDamaged",
                    "# of Structures Destroyed" = "StructuresDestroyed"),
                  selected = "AcresBurned"),
      
      selectInput("var1", "Scatter Plot Y-axis:",
                  c("Acres Burned" = "AcresBurned",
                    "Month" = "Month",
                    "Year" = "Year",
                    "# of Fatalities" = "Fatalities",
                    "# Injured" = "Injuries",
                    "# of Structures Damaged" = "StructuresDamaged",
                    "# of Structures Destroyed" = "StructuresDestroyed"),
                  selected = "Injuries"),
      
      selectInput("var2", "Scatter Plot Dot Size:",
                  c("Acres Burned" = "AcresBurned",
                    "# of Fatalities" = "Fatalities",
                    "# Injured" = "Injuries",
                    "# of Structures Damaged" = "StructuresDamaged",
                    "# of Structures Destroyed" = "StructuresDestroyed"),
                  selected = "Fatalities"),
      
      selectInput("var3", "Bar Plot X-axis:",
                  c("Month" = "Month",
                    "Year" = "Year"),
                  selected = "Year"),
      
      selectInput("var4", "Bar Plot Y-axis:",
                  c("Acres Burned" = "AcresBurned",
                    "# of Fatalities" = "Fatalities",
                    "# Injured" = "Injuries",
                    "# of Structures Damaged" = "StructuresDamaged",
                    "# of Structures Destroyed" = "StructuresDestroyed"),
                  selected = "StructuresDamaged")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Scatter Plot", plotOutput("scatterplot")),
                  tabPanel("Bar Graph", plotOutput(("barplot")))
      )
    )
  )
)
# server logic containing data prep and plotting for the app
server <- function(input, output) {
  # run Ben's data cleaning script plus further cleaning for my needs
  fire_df <- cleanFireData() %>%
    select(AcresBurned, Started, Extinguished, Counties, Fatalities, 
           Injuries, StructuresDamaged, StructuresDestroyed) %>%
    mutate(Started = as.Date(Started, "%Y-%m-%d"), 
           Extinguished = as.Date(Extinguished, "%Y-%m-%d")) %>%
    mutate(Month = month(Started, label=TRUE), Year = year(Started)) %>%
    filter(Year != 1969) %>%
    select(-c(Started, Extinguished))# remove outliers found in data set
  
  # convert 0 and NAs to the mean of AcresBurned  
  fire_df$AcresBurned[fire_df$AcresBurned == 0] <- NA
  fire_df$AcresBurned[is.na(fire_df$AcresBurned)] <- mean(fire_df$AcresBurned, 
                                                          na.rm = TRUE)
  
  # scatterplot showing the desired inputs
  output$scatterplot <- renderPlot(
    ggplot(fire_df, aes(x=fire_df[,input$var], y=fire_df[,input$var1])) +
      geom_point(aes(size=fire_df[,input$var2]), color="dodgerblue") + 
      theme_minimal() +
      labs(x=paste(input$var), y=paste(input$var1), size=paste(input$var2))
  )
  
  # bar plot of the desired inputs
  output$barplot <- renderPlot(
    ggplot(fire_df, aes(x=as.factor(fire_df[,input$var3]), 
                        y=fire_df[,input$var4])) +
      geom_bar(position="dodge", stat="identity", width=0.5, fill="coral") +
      theme_minimal() +
      labs(x=paste(input$var3), y=paste(input$var4))
  )
}

# Deploy the app
#deployApp()

# Run the app
shinyApp(ui = ui, server = server)