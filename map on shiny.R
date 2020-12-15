
## TERM PAPER 
## BAN400 
## FALL 2020 

################################################################################
#     LIBRARIES
################################################################################

# Libraries 
library(dplyr)
library(PxWebApiData) 
library(tidyr)
library(magrittr)
library(purrr)
library(tidyverse)


################################################################################
#                     DATA LOADING 
################################################################################

# Loading API for air pollution by source: 
emission <- ApiData("https://data.ssb.no/api/v0/en/table/08940/",
                    UtslpTilLuft = "5", 
                    UtslpEnergivare = "VT0",
                    UtslpKomp = "A10",
                    ContentsCode = "UtslippCO2ekvival",
                    Tid = c(-1))

# Loading API for average traffic volumes by region
data <- ApiData("https://data.ssb.no/api/v0/en/table/12579/",
                Region = TRUE,
                Kjoretoytype = "0",
                ContentsCode = "GjsnittKjorelengde",
                Tid = c(-1))

# Loading API for total traffic volume for the country as a whole 
traffic <- ApiData("https://data.ssb.no/api/v0/en/table/12579/",
                   Region = "0",
                   Kjoretoytype = "0",
                   ContentsCode = "Kjorelengde",
                   Tid = c(-1))

################################################################################
#                   PRE-PROCESSING 
################################################################################


# Selecting relevant list, removing parantheses information in region column
# and NAs
df <- data.frame(data$`12579: Road traffic volumes, by region, vehicle type, contents and year`) %>%
  na.omit(df) %>%
  map_df(., gsub, pattern = "\\([^()]*\\)", replacement = "") %>%
  select(., c(1,5)) %>%
  rename(., "value: traffic volume (million km)" = value) %>%
  as.data.frame(.)-> df


# Making a vector for average pollution per vehicle based on emission data and 
# traffic volume for the whole country.
pollution <- c(as.numeric((emission[[1]][[6]])*1000))
traffic <- c((traffic[[1]][[5]])*1000000)
traffic <- as.numeric(traffic)

# Average pollution per vehicle  
ave <- pollution/traffic


#######################################################################
# Map of Norway
# install.packages("fhidata") to retrieve map data for Norway
library(fhidata)
library(ggplot2)
library(RColorBrewer)
library(leaflet)
library(plotly)
library(maptools)

#Extracting data from package
current <- as.data.frame(norway_locations_long_current)
map_data <- as.data.frame(norway_map_municips)

#Adding municipality names to map data according to location codes
new_mapdata <- merge(map_data, current, by = "location_code", no.dups = FALSE)

#Simpler mapdata for leaflet
mapdata <- new_mapdata %>% 
  select(c(2,3,9))

#######################################################################


################################################################################
#     SHINY: The initial calculator 
################################################################################

library(shiny)
library(shinydashboard)
library(shinythemes)

#########################################
# User interface 
#########################################

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  h1("Emission calculator"),
  numericInput("carpark", "Enter alteration",
               value = 0, max = 10000, min = -10000),
  selectInput("region", "Select region", df$region),
  
  textOutput("table"),
  mainPanel(
    tableOutput("result"),
    leafletOutput("map")
  )
)

########################################
# Server
########################################

server <- function(input, output){
  inputdata <- reactive({
    data <- data.frame( 
      carpark = as.numeric(input$carpark),
      region = input$region)
    data
  }) 
  output$result <- renderTable({
    data = inputdata()
    resultTable = data.frame(
      Result = "Change in emission for selected region is",
      co2 = ave*(as.numeric((filter(df,region == input$region)[,2])))*input$carpark
    )
    resultTable
  })
  
  output$table <- renderText({
    print("*tons of greenhouse gases per year")
  })
  
  points <- eventReactive(input$region, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)}, ignoreNULL = FALSE)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
  })
}
####################################
# Shiny app
####################################

shinyApp(ui, server)
