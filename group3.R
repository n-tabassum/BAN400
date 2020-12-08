# Group 3
###############################################################################
#                             About the term paper 
###############################################################################
# Title: Effect of vehicles and traffic volume on air emissions in Norway 
# Data reference: ssb.no 
# Due to time and our starting point for R and data programming the calculator 
# has several sources of inaccuracy, e.g. the model does not consider: 
#     - type of vehicle 
#     - type of fuel 
#     - vehicle age
#     - multiple cars for individuals etc. 

#------------------------------------------------------------------------------
# Libraries 
library(dplyr)
library(PxWebApiData) 
library(tidyr)
library(magrittr)
library(plotly)

################################################################################
#                                Loading data 
################################################################################
# Loading API for air pollution by source: 
traffic.emissions <- ApiData("https://data.ssb.no/api/v0/en/table/08940/",
                             UtslpTilLuft = "5", 
                             UtslpEnergivare = "VT0",
                             UtslpKomp = "A10",
                             ContentsCode = "UtslippCO2ekvival",
                             Tid = c(-1))

# Loading API for average traffic volumes by region
traffic.volume <- ApiData("https://data.ssb.no/api/v0/en/table/12579/",
                          Region = TRUE,
                          Kjoretoytype = "0",
                          ContentsCode = "GjsnittKjorelengde",
                          Tid = c(-1))

# Loading API for total traffic volume for the country as a whole 
total.volume <- ApiData("https://data.ssb.no/api/v0/en/table/12579/",
                        Region = "0",
                        Kjoretoytype = "0",
                        ContentsCode = "Kjorelengde",
                        Tid = c(-1))

###############################################################################
#                            Wrangle data 
###############################################################################
# Converting lists to data frames
# Emission
df.emissions <- data.frame(
  traffic.emissions$`08940: Greenhouse gases, by source (activity), energy product, pollutant, contents and year`)

# Total traffic volume for the country as a whole 
df.tot.volume <- data.frame(total.volume$`12579: Road traffic volumes, by region, vehicle type, contents and year`)

# Data frame for average traffic volume by region
df.volume <- data.frame(traffic.volume$`12579: Road traffic volumes, by region, vehicle type, contents and year`) %>%
  na.omit(df.volume)

# Formatting the data frame for traffic volume by region. Starting by 
region <- df.volume$region

# Removing everything inside the parentheses of region values
region <- gsub("\\s*\\([^\\)]+\\)","",as.character(region))

# Making it into a data frame 
region <- data.frame(region)

# Merge it with the initial data frame 
df.volume <- cbind(df.volume,region)

# Rearranging data frame and selecting only three columns 
df.volume <- df.volume[,c(6,4,5)]

# Remove data frame region 
rm(region)

#------------------------------------------------------------------------------
# Creating vectors, making sure they are numeric 
# 1)
emission <- c ((df.emissions[[6]])*1000)
# 2)
tot.volume <- c(as.numeric(df.tot.volume[[5]])*1000000)


###############################################################################
#                       Creating visualization
###############################################################################
# The figure shows total air emission due to road traffic in Norway. 
# We had planned to visualize the increase or decrease that we have calculated
# using the function - but could not add that modification in due time. 

figure <- function(x) {
  x <- emission
  fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = emission,
    title = list(text = "Emissions to Air due to Road Traffic \n in Norway (tons CO2-equivalents)"),
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      axis = list(range = list(NULL, 12000000), tickwidth = 1, tickcolor = "black"),
      bar = list(color = NULL, barwidth = 1),
      bgcolor = "white",
      borderwidth = 1,
      bordercolor = "black",
      steps = list(
        list(range = c(0, 4000000), color = "yellow"),
        list(range = c(4000000, 9000000), color = "orange"),
        list(range = c(9000000, 12000000), color = "red"))))
  return(fig)
}

figure()


###############################################################################
#                            Creating the function 
###############################################################################

# Emission calculation function:
emission.calculation <- function(x,y){
  y <- readline(prompt = "Enter alteration of carpark: ")
  x <- readline(prompt = "Enter region: ")
  r <- filter(df.volume,region == x)[,1]
  v <-  as.numeric(filter(df.volume, region == x)[,3])
  a <- (((emission/tot.volume)*v)*(as.numeric(y)))
  options(digits = 2)
  if (y == 0){
    return(cat("The average vehicle in",r, "emits", ((emission/tot.volume)*v),
               "tons of greenhouse gases per year."))
  } else if (y > 0){
    return(cat("An addition of",y,"vehicles in", r, "will increase the road traffic
               emissions by",a,"tons of greenhouse gases per year. This equals a",
               ((a/emission)*100),"% 
               increase in total emissons from road traffic in Norway."))
  } else if (y < 0) {
    return(cat("A reduction of",(as.numeric(y)*(-1)),"vehicles in", r,
               "will decrease the road traffic emissions 
                 by",a*(-1),"tons of greenhouse gases per year. This equals a",
               ((-1)*(a/emission)*100),"%
                 decrease in total emissons from road traffic in Norway."))
  }
}

# Try it: 
emission.calculation()


