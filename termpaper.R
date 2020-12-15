
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
library(ggplot2)
library(ggthemes)


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
                   Region = TRUE,
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
  rename(., "average volume per vehicle (km)" = value) %>%
  as.data.frame(.)-> df

df.vol <-data.frame(traffic$`12579: Road traffic volumes, by region, vehicle type, contents and year`) %>%
  na.omit(.) %>%
  map_df(., gsub, pattern = "\\([^()]*\\)", replacement = "") %>%
  select(., c(1,5))%>%
  rename(., "total traffic volume (mill. km.)" = value) %>%
  as.data.frame(.) -> df.vol

df <- merge.data.frame(df, df.vol)

# Making a vector for average pollution per vehicle based on emission data and 
# traffic volume for the whole country.
ave <- (as.numeric((emission[[1]][[6]])*1000))/
  (as.numeric(filter(df, region == "The whole country")[,3])*1000000)
                 
################################################################################
#     SHINY: The emission calculator 
################################################################################
# User interface 
#########################################
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  h1("Emission calculator"),
  sidebarPanel(
    numericInput("carpark", "Enter alteration",
                 value = 0),
    selectInput("region", "Select region", df$region),
    
    tableOutput("result"),
    textOutput("table")),
  
  mainPanel(
    textOutput("explanation"),
    
    
    plotOutput("plot")
  )
)

########################################
# Server
########################################

server <- function(input, output){
  inputdata <- reactive({
    req(input$carpark)
    data <- data.frame( 
      carpark = as.numeric(input$carpark),
      region = input$region)
    data
  }) 
  output$result <- renderTable({
    data = inputdata()
    resultTable = data.frame(
      Result = "Change in emission for selected region is",
      CO2 = ave*(as.numeric(filter(df, region == input$region)[,2])*input$carpark)
    )
    resultTable
  })
  
  output$table <- renderText({
    print("* in tonnes of greenhouse gases per year")
  })
  
  #explanation text
  output$explanation <- renderText({
    req(input$carpark)
    init <- ave*(as.numeric(filter(df, region == input$region)[,3])*1000000)
    co2 <- ave*(as.numeric(filter(df, region == input$region)[,2])*input$carpark)
    new.co2 <- (init + co2)
    percent <- ((new.co2-init)/init)*100
    percent <- round(percent, digits = 2)
    
    if (input$carpark > 0) 
      print(paste("An addition of", input$carpark,
                  "vehicles in" , input$region, ", will increase emissions by",
                  percent, "% per year."))
    
    else if (input$carpark < 0)
      print(paste("A reduction of", (-1)*input$carpark,
                  "vehicles in", input$region, ", will decrease emissions by",
                  percent*(-1), "% per year."))
    else if (input$carpark == 0)
      print(paste(NULL))
  })
  
  #plot
  output$plot <- renderPlot({
    req(input$carpark)
    co2 <- ave*(as.numeric(filter(df, region == input$region)[,2])*input$carpark)
    init <- ave*(as.numeric(filter(df, region == input$region)[,3])*1000000)
    new.co2 <- (init + co2)
    
    ggplot() +
      aes(x = input$region, y = new.co2) +
      labs(title = "Tonnes of greenhouse gases per year:",
           caption = ,"Based on latest data from SSB",
           x = "Region",
           y = "tonnes of CO2-equivalents") +
      geom_col(width = 0.2,
               fill = ifelse(input$carpark < 0, "seagreen2", "rosybrown3")) +
      geom_col(aes(x = input$region, y = init),
              width = 0.2,
              fill = "lightblue3") +
      geom_label(label = round(new.co2, digits = 0), y = new.co2) +
      geom_label(label = round(init, digits = 0), y = init) +
      theme_classic()
    
  }) 
}




####################################
# Shiny app
####################################

shinyApp(ui, server)


