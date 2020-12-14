
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
#Static Map of Norway

install.packages("fhidata")
library(fhidata)
library(ggplot2)


#Extracting data from package
current <- as.data.frame(norway_locations_long_current)
map_data <- as.data.frame(norway_map_municips)
new_mapdata <- merge(map_data, current, by = "location_code", no.dups = FALSE)

#Static map divided my municipalities
static_map <- ggplot(mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = new, color = "black") +
  theme_void() + 
  coord_quickmap()
static_map

#######################################################################