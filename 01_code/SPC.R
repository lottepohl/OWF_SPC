# Title: Wrangling and combining all geospatial data for Submarine Power Cables in the North Sea
# Author: Lotte Pohl (Flanders Marine Institute, VLIZ), lotte.pohl@gmail.com
# Date: 2024-04-08

# Introduction ------------------------------------------------------------

# For each country bordering the (Southern) North Sea, the respective shapefiles are queried, 
# or their download link is provided. They are then transformed in the CRS 'EPSG:4326' and unified with standardised column names.
# These column names are
# country: Country name
# geometry: The geospatial information of the cable
# status: status of the cable. Should be 'in Use'
# name: Name of the cable (if applicable)
# id: Cable identifier (if applicable)
# owner: Cable owner company (if applicable)
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
# library(devtools)
# devtools::install_github("lifewatch/mregions2")
library(mregions2) # to query geospatial data (EEZs, countries)

rm(list = ls()) # remove all variables

this_crs <- 'EPSG:4326'
file_suffix <- "_crs_EPSG4326"
these_column_names <- c("country", "status", "name", "id", "owner", "technical_info", "comment")

# 01. Belgium -------------------------------------------------------------------
# Data Source: https://spatial.naturalsciences.be/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?3&filter=false
# Name: od_nature:MUMM_energy_cables_ETRS89_2, data download option = '.shp'

# List all files with the string "MUMM" in the repository, these are the Belgian submarine power cable shapefiles
matching_files <- list.files(recursive = TRUE, full.names = TRUE, pattern = "MUMM")
matching_files

# rename the file and copy to a new directory
for (file in matching_files) {
  new_name <- gsub("MUMM_energy_cables_ETRS89_2", paste0("BE", file_suffix), basename(file))  # Replace "MUMM" with "BE_4326" in the filename
  destination_path <- file.path(getwd(), "00_data", new_name)  # Construct the destination path
  file.copy(file, destination_path)
}

BE_SPC <- sf::st_read(file.path(getwd(), "00_data", paste0("BE", file_suffix, ".shp")))

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

# remove unnecessary variables
rm(GER_wfs, GER_request, GER_url, GER_client, GER_url, GER_layers)

# check class and CRS
class(GER_SPC)
sf::st_crs(GER_SPC)

# set crs
sf::st_crs(GER_SPC) <- this_crs

# # # write .shp file
# sf::st_write(obj = GER_SPC, dsn = file.path(getwd(), "00_data", paste0("GER", file_suffix)), append = F)


## data wrangling ----------------------------------------------------

GER_SPC_orig <- GER_SPC # for comparison

GER_SPC <- 
  GER_SPC %>% 
    dplyr::filter(status == 'inUse') %>% # only currently used cables
    dplyr::mutate(country = "GER") # add column with country name

# 03. France --------------------------------------------------------------

# Data Source: https://odre.opendatasoft.com/explore/dataset/lignes-souterraines-rte-nv/map/?disjunctive.etat&disjunctive.tension&location=8,49.38952,-4.33136&basemap=jawg.light

# List all files with the string "lignes-souterraines" in the repository, these are the French submarine power cable shapefiles
matching_files <- list.files(recursive = TRUE, full.names = TRUE, pattern = "lignes-souterraines")
matching_files

# rename the file and copy to a new directory
for (file in matching_files) {
  new_name <- gsub("lignes-souterraines-rte-nv", paste0("FR", file_suffix), basename(file))  # Replace "MUMM" with "BE_4326" in the filename
  destination_path <- file.path(getwd(), "00_data", new_name)  # Construct the destination path
  file.copy(file, destination_path)
}

FR_SPC <- sf::st_read(file.path(getwd(), "00_data", paste0("FR", file_suffix, ".shp")))

# check CRS
st_crs(FR_SPC)

## data wrangling ----------------------------------------------------------

FR_SPC_orig <- FR_SPC # for comparison

# all subterrestrial cables need to be removed, thus geospatial data for France as a country is required
FR <- mregions2::gaz_search(17) %>% mregions2::gaz_geometry()
sf::st_crs(FR)

FR_SPC <- 
  sf::st_difference(FR_SPC, FR) %>%
  dplyr::filter(etat == 'EN EXPLOITATION')


# 04. Netherlands ---------------------------------------------------------

# List all files with the string "Cobra" or "electra" in the repository, these are part of the Dutch submarine power cable shapefiles
matching_files <- list.files(recursive = TRUE, full.names = TRUE, pattern = "Cobra|electra")
matching_files

# open the two .shp files
NL_1_SPC <- sf::st_read(file.path(getwd(), matching_files[grep("\\.shp$", matching_files)][1]))
NL_2_SPC <- sf::st_read(file.path(getwd(), matching_files[grep("\\.shp$", matching_files)][2]))


## data wrangling --------------------------------------------------------

NL_1_orig -> NL_1_SPC # for comparison
NL_2_orig -> NL_2_SPC

# create standardised columns
NL_1_SPC <- 
  NL_1_SPC %>% 
    dplyr::rename(name = Project,
                  technical_info = Comment) %>%
    dplyr::mutate(country = "NL",
                  status = "in_use",
                  id = NA,
                  owner = NA,
                  comment = NA) %>% 
    dplyr::select(any_of(these_column_names))

NL_2_SPC <- 
  NL_2_SPC %>%
    dplyr::rename(id = KABEL_NR,
                  name = NAAM,
                  owner = EIGENAAR,
                  status = STATUS,
                  comment = OMSCHRIJVI,
                  technical_info= KABEL_TYPE) %>% 
    dplyr::mutate(country = "NL",
                  status = 
                      ifelse(status == 'Ingebruik', 'in_use',
                             ifelse(status == 'Toekomstig', 'planned', status))) %>%
    dplyr::select(any_of(these_column_names))

NL_SPC <- rbind(NL_1_SPC, NL_2_SPC)

# common_column_names <- intersect(names(NL_1_SPC), names(NL_2_SPC))
# 
# NL_SPC <- sf::st_join(x = NL_1_SPC, y = NL_2_SPC, by = common_column_names)



# overview map ------------------------------------------------------------

ggplot() +
  geom_sf(data = BE_SPC) +
  geom_sf(data = GER_SPC) +
  # geom_sf(data = FR) +
  geom_sf(data = FR_SPC) 
