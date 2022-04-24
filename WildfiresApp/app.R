# Script written by Alejandro Cepeda

# libraries
#install.packages(c('shiny','shinythemes','thematic','tidyverse','lubridate','rsconnect'))
library(shiny)
library(shinythemes)
library(thematic)
library(tidyverse)
library(lubridate)
library(rsconnect)

# Benjamin's data cleaning script as a function
cleanFireData <- function() {
  
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
  theme = shinytheme("yeti"),
  titlePanel("California Forest Fire Analysis"),
  # set up two tab panels
  tabsetPanel(
    tabPanel("Scatter Plot", fluid = TRUE,
             sidebarLayout(
               # sidebar inputs for scatterplot
               sidebarPanel(
                 selectInput("var", "X-axis:",
                             c("Acres Burned" = "AcresBurned",
                               "# of Fatalities" = "Fatalities",
                               "# Injured" = "Injuries",
                               "# of Structures Damaged" = "StructuresDamaged",
                               "# of Structures Destroyed" = "StructuresDestroyed"),
                             selected = "AcresBurned"),
                 
                 selectInput("var1", "Y-axis:",
                             c("Acres Burned" = "AcresBurned",
                               "# of Fatalities" = "Fatalities",
                               "# Injured" = "Injuries",
                               "# of Structures Damaged" = "StructuresDamaged",
                               "# of Structures Destroyed" = "StructuresDestroyed"),
                             selected = "Injuries"),
                 
                 selectInput("var2", "Dot Size:",
                             c("Acres Burned" = "AcresBurned",
                               "# of Fatalities" = "Fatalities",
                               "# Injured" = "Injuries",
                               "# of Structures Damaged" = "StructuresDamaged",
                               "# of Structures Destroyed" = "StructuresDestroyed"),
                             selected = "Fatalities")
               ),
               mainPanel(
                 # display scatter plot in main panel
                 plotOutput("scatterplot")
               )
             )
    ),
    tabPanel("Bar Graph", 
             sidebarLayout(
               # sidebar inputs for the bar graph
               sidebarPanel(
                 selectInput("var3", "X-axis:",
                             c("Month" = "Month",
                               "Year" = "Year"),
                             selected = "Year"),
                 
                 selectInput("var4", "Y-axis:",
                             c("Acres Burned" = "AcresBurned",
                               "# of Fatalities" = "Fatalities",
                               "# Injured" = "Injuries",
                               "# of Structures Damaged" = "StructuresDamaged",
                               "# of Structures Destroyed" = "StructuresDestroyed"),
                             selected = "AcresBurned")
               ),
               mainPanel(
                 plotOutput("bargraph")
               )
             )
    )
  )
)

# server logic containing data prep and plotting for the app
server <- function(input, output) {
  # run Ben's data cleaning script plus further cleaning for my needs
  fire_df <- cleanFireData() %>%
    select(AcresBurned, CalFireIncident, Started, Extinguished, 
           Fatalities, Injuries, StructuresDamaged, StructuresDestroyed) %>%
    mutate(AcresBurned = as.numeric(AcresBurned),
           Started = as.Date(Started, "%Y-%m-%d"),
           Extinguished = as.Date(Extinguished, "%Y-%m-%d")) %>%
    mutate(Month = month(Started, label=TRUE), Year = year(Started)) %>%
    filter(Year != 1969) %>% # remove Year outliers found in data set
    select(-c(Started, Extinguished))
  
  # convert 0 and NAs to the mean of AcresBurned  
  fire_df$AcresBurned[fire_df$AcresBurned == 0] <- NA
  fire_df$AcresBurned[is.na(fire_df$AcresBurned)] <- mean(fire_df$AcresBurned, 
                                                          na.rm = TRUE)
  # create season variable for scatter plot color use joined with original data frame
  fire_df$Month <- as.numeric(fire_df$Month)
  
  # setting month range by seasons
  spring <- c(03,04,05)
  fall <- c(09,10,11)
  summer <- c(06,07,08)
  winter <- c(12,01,02)
  
  seasons <- data.frame(
    Season = c("spring","spring","spring", "fall","fall","fall", 
               "summer","summer","summer", "winter","winter","winter"),
    Month = c(spring, fall, summer, winter)
  )
  # joining seasons by month
  fire_df <- fire_df %>%
    full_join(seasons, fire_df, by = "Month") %>%
    # setting variables to desired format
    mutate(Month = month(Month, label=TRUE),
           Season = as.factor(Season))
  
  # scatter plot showing the desired inputs
  output$scatterplot <- renderPlot(
    ggplot(fire_df, aes(x=fire_df[,input$var], y=fire_df[,input$var1])) +
      geom_point(aes(color=Season, size=fire_df[,input$var2])) + 
      labs(title=paste(input$var, "vs.", input$var1),
           x=paste(input$var), y=paste(input$var1), 
           size=paste(input$var2)) +
      scale_x_continuous(labels = scales::comma)
  )
  
  # bar graph of the desired inputs
  output$bargraph <- renderPlot(
    ggplot(fire_df, aes(x=as.factor(fire_df[,input$var3]), 
                        y=fire_df[,input$var4], fill=Season)) +
      geom_bar(position="dodge", stat="identity", width=0.7) +
      labs(title=paste(input$var4, "by", input$var3),
           x=paste(input$var3), y=paste(input$var4)) +
      scale_y_continuous(labels = scales::comma)
  )
  
}

# set plot theme to minimal
theme_set(theme_minimal())
# have the shiny theme match the ggplot theme
thematic_shiny()

# Run the app
shinyApp(ui = ui, server = server)

# deploy the app
#deployApp()
