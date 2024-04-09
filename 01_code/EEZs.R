# Title: Wrangling and combining all geospatial data for Exclusive Economic Zones (EEZ) of countries bordering in the North Sea
# Author: Lotte Pohl (Flanders Marine Institute, VLIZ), lotte.pohl@gmail.com
# Date: 2024-04-09

# Introduction ------------------------------------------------------------

# For each country bordering the (Southern) North Sea, the respective geospatial EEZ data are queried via the `mregions2` R package.
# They are then transformed in the CRS 'EPSG:4326' and unified with standardised column names.

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

# The end result is one .shp file 'SPC.shp' that contains all geospatial information of currently 
# (as of April, 2024) installed submarine power cables (SPC) in the (Southern) North Sea.
# The resulting shapefile will be saved in the folder '03_results'.

# The relevant countries are: Belgium (BE), Germany (GER), United Kingdom (UK), Netherlands (NL), Denmark (DK), France (FR)


# Workspace setup ---------------------------------------------------------

library(ows4R) # WFS services
library(httr) # http requests
library(dplyr) # data wrangling
library(sf) # geospatial data operations
library(ggplot2) # plots
library(leaflet) # interactive maps
# library(devtools)
# devtools::install_github("lifewatch/mregions2")
library(mregions2) # to query geospatial data (EEZs, countries)

rm(list = ls()) # remove all variables

this_crs <- "EPSG:4326"
file_suffix <- "_crs_EPSG4326"
these_column_names <- c("country", "status", "name", "id", "owner", "technical_info", "voltage", "comment")


# 00. functions -----------------------------------------------------------

add_missing_cols <- function(col_names, SPC_df){
  # add column from `these_column_names` that are not yet present
  missing_columns <- base::setdiff(these_column_names, names(SPC_df))
  
  
  # Create missing columns with NA values
  for (col_name in missing_columns) {
    SPC_df[[col_name]] <- NA
  }
  
  result <- 
    SPC_df %>% 
    dplyr::select(all_of(these_column_names))
}

# 01. query EEZs -----------------------------------------------------
# The mrgid for `gaz_search()` for each country's EEZ is found here: https://www.marineregions.org/gazetteer.php?p=search

# Europe_EEZ <- mregions2::gaz_search(23734) %>% mregions2::gaz_geometry() #not needed here
BE_EEZ <- mregions2::gaz_search(2393) %>% mregions2::gaz_geometry()
UK_EEZ <- mregions2::gaz_search(5696) %>% mregions2::gaz_geometry()
IRL_EEZ <- mregions2::gaz_search(5681) %>% mregions2::gaz_geometry()
NL_EEZ <- mregions2::gaz_search(5668) %>% mregions2::gaz_geometry()
DK_EEZ <- mregions2::gaz_search(5674) %>% mregions2::gaz_geometry()
FR_EEZ <- mregions2::gaz_search(5677) %>% mregions2::gaz_geometry()
SE_EEZ <- mregions2::gaz_search(5694) %>% mregions2::gaz_geometry()
# PL <- mregions2::gaz_search(2244) %>% mregions2::gaz_geometry()
# CZ <- mregions2::gaz_search(2158) %>% mregions2::gaz_geometry()
# AUT <- mregions2::gaz_search(2146) %>% mregions2::gaz_geometry()
# SL <- mregions2::gaz_search(2189) %>% mregions2::gaz_geometry()
NO_EEZ <- mregions2::gaz_search(5686) %>% mregions2::gaz_geometry()
# CH <- mregions2::gaz_search(2179) %>% mregions2::gaz_geometry()
# LUX <- mregions2::gaz_search(2233) %>% mregions2::gaz_geometry()

# Belgium (BE), Germany (GER), United Kingdom (UK), Ireland (IRL), Netherlands (NL), Denmark (DK), France (FR),
# Sweden (SE). Poland (PL), Czech Republic (CZ), Austria (AUT), Slovakia (SL), Norway (NO), Switzerland (CH), Luxembourg (LUX)

# 02. combine data --------------------------------------------------------

countries_4326 <- 
  rbind(AUT,
        BE,
        CH,
        CZ,
        DK,
        FR,)

