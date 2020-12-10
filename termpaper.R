
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
library(shiny)
library(shinythemes)

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
ave <- (as.numeric((emission[[1]][[6]])*1000))/(as.numeric((traffic[[1]][[5]])*1000000))

################################################################################
#     SHINY: The initial calculator 
################################################################################
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
    tableOutput("result")
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
  
}

####################################
# Shiny app
####################################

shinyApp(ui, server)
