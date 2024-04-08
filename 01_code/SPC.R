# Title: Wrangling and combining all geospatial data for Submarine Power Cables in the North Sea
# Author: Lotte Pohl (Flanders Marine Institute, VLIZ), lotte.pohl@gmail.com
# Date: 2024-04-08

# Introduction ------------------------------------------------------------

# For each country bordering the (Southern) North Sea, the respective shapefiles are queried, 
# or their download link is provided. They are then transformed in the CRS 'EPSG:4326' and unified.
# The end result is one .shp file 'SPC.shp' that contains all geospatial information of currently 
# (as of April, 2024) installed submarine power cables (SPC) in the (Southern) North Sea.
# The resulting shapefile will be saved in the folder '03_results'

# The relevant countries are: Belgium (BE), Germany (GER), United Kingdom (UK), Netherlands (NL), Denmark (DK), France (FR)


# Workspace setup ---------------------------------------------------------

library(ows4R) # WFS services
library(httr) # http requests
library(dplyr) # data wrangling
library(sf) # geospatial data operations
library(ggplot2) # plots

rm(list = ls()) # remove all variables

this_crs <- 'EPSG:4326'
file_suffix <- "_crs_EPSG4326.shp"

# 01. Belgium -------------------------------------------------------------------
# Data Source: https://spatial.naturalsciences.be/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?3&filter=false
# Name: od_nature:MUMM_energy_cables_ETRS89_2, data download option = '.shp'

# List all files with the string "MUMM" in the repository, these are the Belgian submarine power cable shapefiles
matching_files <- list.files(recursive = TRUE, full.names = TRUE, pattern = "MUMM")
matching_files

# rename the file and copy to a new directory
for (file in matching_files) {
  new_name <- gsub("MUMM_energy_cables_ETRS89_2", "BE_crs_EPSG4326", basename(file))  # Replace "MUMM" with "BE_4326" in the filename
  destination_path <- file.path(getwd(), "00_data", new_name)  # Construct the destination path
  file.copy(file, destination_path)
}

BE_SPC <- sf::st_read(file.path(getwd(), "00_data", paste0("BE", file_suffix)))

st_crs(BE_SPC)

# 02. Germany -------------------------------------------------------------

# WFS URL
GER_wfs <- "https://gdi.bsh.de/en/mapservice/Industrial-and-Production-Facilities-of-the-Continental-Shelf-Information-System-WFS"

# make client
GER_client <- WFSClient$new(GER_wfs, 
                                   serviceVersion = "2.0.0") # from ows4R
# get layers
GER_layers <- GER_client$getFeatureTypes(pretty = TRUE) # from ows4R

# make http request
GER_url <- httr::parse_url(GER_wfs) 
GER_url$query <- list(service = "wfs",
                     version = "2.0.0", # optional
                     request = "GetFeature",
                     typename = GER_layers$name[2])
GER_request <- httr::build_url(GER_url)
GER_request

# read geospatial data
GER_SPC <- sf::st_read(GER_request)

# remove unneccessary variables
rm(GER_wfs, GER_request, GER_url, GER_client, GER_url, GER_layers)

# check class and CRS
class(GER_SPC)
sf::st_crs(GER_SPC)

# set crs
sf::st_crs(GER_SPC) <- this_crs

# # write .shp file
sf::st_write(obj = GER_SPC, dsn = file.path(getwd(), "00_data", paste0("GER", file_suffix)), append = F)



# overview map ------------------------------------------------------------

ggplot() +
  geom_sf(data = BE_SPC) +
  geom_sf(data = GER_SPC)
