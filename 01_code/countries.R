# Title: Wrangling and combining all geospatial data for countries bordering the North Sea
# Author: Lotte Pohl (Flanders Marine Institute, VLIZ), lotte.pohl@gmail.com
# Date: 2024-04-09

# Introduction ------------------------------------------------------------

# For each country bordering the (Southern) North Sea, the respective shapefiles are queried, 
# or their download link is provided. They are then transformed in the CRS 'EPSG:4326' and unified with standardised column names.

#  TO DO: update column names and descriptions
# These column names are
# country: Country name
# geometry: The geospatial information of the cable
# status: status of the cable. Should be 'inUse'. Can be one of: 'inUse', 'Unknown', 'Approved', 'OutOfUse', 'UnderConstruction', 'Planned'
# name: Name of the cable (if applicable)
# id: Cable identifier (if applicable)
# owner: Cable owner company (if applicable)
# voltage: The cable's operating voltage
# technical_info: Technical specifications about the cable (if applicable)
# comment: Any other relevant information

# The end result are two .shp file 'countries_4326.shp' and 'countries_3035.shp' that contain all geospatial information of countried bordering
# the (Southern) North Sea.
# The resulting shapefiles will be saved in the folder '03_results'.

# The relevant countries are: Belgium (BE), Germany (GER), United Kingdom (UK), Ireland (IRL), Netherlands (NL), Denmark (DK), France (FR),
# Sweden (SE). Poland (PL), Czech Republic (CZ), Austria (AUT), SLovakia (SL), Norway (NO), Switzerland (CH)


# Workspace setup ---------------------------------------------------------

rm(list = ls()) # remove all variables

library(ows4R) # WFS services
library(httr) # http requests
library(dplyr) # data wrangling
library(sf) # geospatial data operations
library(ggplot2) # plots
library(leaflet) # interactive maps
# library(devtools)
# devtools::install_github("lifewatch/mregions2")
library(mregions2) # to query geospatial data (EEZs, countries)

this_crs <- "EPSG:4326"
# file_suffix <- "_crs_EPSG4326"
these_column_names <- c("country", "northSeaBorder", "mrgid", "comment")


# 00. functions -----------------------------------------------------------

add_missing_cols <- function(col_names, country_df){
  # add column from `these_column_names` that are not yet present
  missing_columns <- base::setdiff(these_column_names, names(country_df))
  
  
  # Create missing columns with NA values
  for (col_name in missing_columns) {
    country_df[[col_name]] <- NA
  }
  
  result <- 
    country_df %>% 
    dplyr::select(all_of(these_column_names))
}


# 01. query countries -----------------------------------------------------
# The mrgid for `gaz_search()` for each country is found here: https://www.marineregions.org/gazetteer.php?p=search

Europe <- mregions2::gaz_search(23734) %>% mregions2::gaz_geometry()
BE <- mregions2::gaz_search(14) %>% mregions2::gaz_geometry()
UK <- mregions2::gaz_search(2208) %>% mregions2::gaz_geometry()
IRL <- mregions2::gaz_search(2114) %>% mregions2::gaz_geometry()
NL <- mregions2::gaz_search(15) %>% mregions2::gaz_geometry()
DK <- mregions2::gaz_search(2157) %>% mregions2::gaz_geometry()
FR <- mregions2::gaz_search(17) %>% mregions2::gaz_geometry()
SE <- mregions2::gaz_search(2180) %>% mregions2::gaz_geometry()
PL <- mregions2::gaz_search(2244) %>% mregions2::gaz_geometry()
CZ <- mregions2::gaz_search(2158) %>% mregions2::gaz_geometry()
AUT <- mregions2::gaz_search(2146) %>% mregions2::gaz_geometry()
SL <- mregions2::gaz_search(2189) %>% mregions2::gaz_geometry()
NO <- mregions2::gaz_search(2252) %>% mregions2::gaz_geometry()
CH <- mregions2::gaz_search(2179) %>% mregions2::gaz_geometry()
LUX <- mregions2::gaz_search(2233) %>% mregions2::gaz_geometry()

# Belgium (BE), Germany (GER), United Kingdom (UK), Ireland (IRL), Netherlands (NL), Denmark (DK), France (FR),
# Sweden (SE). Poland (PL), Czech Republic (CZ), Austria (AUT), Slovakia (SL), Norway (NO), Switzerland (CH), Luxembourg (LUX)

# 02. combine data --------------------------------------------------------

countries_4326 <- 
  rbind(AUT,
        BE,
        CH,
        CZ,
        DK,
        FR,
        IRL,
        LUX,
        NL,
        NO,
        PL,
        SE,
        SL,
        UK)

countries_3035 <- 


# 03. write files ---------------------------------------------------------

sf::st_write(obj = Europe_4326, dsn = file.path(getwd(), "02_results", "Europe_4326.shp"), append = F)
sf::st_write(obj = Europe_3035, dsn = file.path(getwd(), "02_results", "SPC_3035.shp"), append = F)
