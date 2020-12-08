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
  select(., c(1,2,5)) %>%
  rename(., "value: traffic volume (million km)" = value) %>%
  as.data.frame(.)-> df


# Making a vector for average pollution per vehicle based on emission data and 
# traffic volume for the whole country.
pollution <- c(as.numeric((emission[[1]][[6]])*1000))
traffic <- c((traffic[[1]][[5]])*1000000)
traffic <- as.numeric(traffic)


################################################################################
#                  CALCULATOR
################################################################################
# Emission calculation function:
emission.calculation <- function(x,y){
  y <- readline(prompt = "Enter alteration of carpark: ")
  x <- readline(prompt = "Enter region: ")
  r <- filter(df,region == x)[,1]
  v <-  as.numeric(filter(df, region == x)[,3])
  a <- (((pollution/traffic)*v)*(as.numeric(y)))
  options(digits = 2)
  if (y == 0){
    return(cat("The average vehicle in",r, "emits", ((pollution/traffic)*v),
               "tons of greenhouse gases per year."))
  } else if (y > 0){
    return(cat("An addition of",y,"vehicles in", r, "will increase the road traffic
               emissions by",a,"tons of greenhouse gases per year. This equals a",
               ((a/pollution)*100),"% 
               increase in total emissons from road traffic in Norway."))
  } else if (y < 0) {
    return(cat("A reduction of",(as.numeric(y)*(-1)),"vehicles in", r,
               "will decrease the road traffic emissions 
                 by",a*(-1),"tons of greenhouse gases per year. This equals a",
               ((-1)*(a/pollution)*100),"%
                 decrease in total emissons from road traffic in Norway."))
  }
}

# Try it: 
emission.calculation()

