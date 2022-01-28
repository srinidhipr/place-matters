library(maps) # Loading in required packages
library(mapdata)
library(tidyverse) 
library(rgdal) 
library(shiny)
library(shinythemes)
library(leaflet)

setwd("/Users/srinidhi/Desktop/PSCI 207/Problem Set 6") # Sets working directory

# Beginning the app
server <- function(input, output) {
  
  youthrisk <- read.csv("philly-youth-risk.csv") # Read in data
  
  zipcodes <- readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly", encoding = "UTF-8") # Reading in shapefile
  zipcodes@data <- data.frame(zipcodes@data, youthrisk[match(zipcodes@data$CODE, youthrisk$CODE),]) # Left-join youthrisk 
  # with shapefiles
  
  # Make pop-up to display data of particular zip code when clicked
  zipcode_popup <- paste0("<strong>ZIP Code: </strong>", 
                          zipcodes@data$CODE, 
                          "<br><strong> Risk Score: </strong>", 
                          zipcodes@data$Risk)
  
  # Creating continuous color scheme
  contpal <- colorNumeric(palette = c("#fff7ec", "#7f0000"),
                          domain = zipcodes@data$Risk)
  
  # Creating leaflet map with legend
  map <- leaflet(zipcodes) %>% 
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 0.2, 
                fillOpacity = .8,
                color = ~contpal(Risk),
                weight = 1, 
                popup = zipcode_popup) %>%
    addLegend("bottomright", 
              pal = contpal,
              na.label = "No Data",
              values = ~Risk,
              title= "Risk Scores",
              opacity = 1) 
  
  # Tells shiny to render map in app
  output$map <- renderLeaflet(map)
  
  # CREATING GRAPH
  
  # Creating scatterplot of poverty vs. crime
  output$plot1 <- renderPlot({
    par(bg = "#F5F6F7")
    plot(x = youthrisk$Poverty, 
         y = youthrisk$Crime,
         pch = 18,
         col = "#B53C1A",
         cex = 1.5,
         main = "Shooting Victims by Percent of Families in Poverty",
         bty = "l",
         xlab = "Percent of families with children below the poverty line",
         ylab = "Shooting victims per 10,000",
         xaxt = "n"
    )
    
    axis(1, at = seq(0, 60, 10),
         c("0%", "10%", "20%", "30%", "40%", "50%", "60%"))
    
    abline(h = seq(0, 25, 5),
           col = "gray",
           lwd = .5)
    
  })
}

## UI Section

# These two lines are a workaround for my NA label in the map legend being incorrectly positioned, obscuring
# the rest of the legend. This fix adds a line break that positions the NA label correctly.  
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))

ui <- shinyUI(fluidPage(
  HTML(html_fix), # calls fix
  theme = shinytheme("sandstone"), # sets theme
  navbarPage(
    "Place Matters",
    # FIRST TAB
    tabPanel(
      "Risk Score Map",
      headerPanel("Youth Risk Scores of PHL ZIP Codes"),
      # map is called here
      leafletOutput("map",
                    width = "100%",
                    height = "600px")
      
    ),
    # SECOND TAB
    tabPanel(
      "Poverty vs. Crime",
      headerPanel("Examine the relationship between poverty and crime."),
      # plot is called here
      plotOutput("plot1")
    )
  )
))
shinyApp(ui = ui, server = server) # Combines server and UI scripts

