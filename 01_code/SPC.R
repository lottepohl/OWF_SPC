# Title: Wrangling and combining all geospatial data for Submarine Power Cables in the North Sea
# Author: Lotte Pohl (Flanders Marine Institute, VLIZ), lotte.pohl@gmail.com
# Date: 2024-04-08

# Introduction ------------------------------------------------------------

# For each country bordering the (Southern) North Sea, the respective shapefiles are queried, 
# or their download link is provided. They are then transformed in the CRS 'EPSG:4326' and unified with standardised column names.
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


# 01. Belgium -------------------------------------------------------------------
# Data Source: https://spatial.naturalsciences.be/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?3&filter=false
# Name: od_nature:MUMM_energy_cables_ETRS89_2, data download option = '.shp'

this_country <- "BE" # set country variable to Belgium

## open and copy files -----------------------------------------------------

# List all files with the string "MUMM" in the repository, these are the Belgian submarine power cable shapefiles
matching_files <- list.files(recursive = TRUE, full.names = TRUE, pattern = "MUMM")
matching_files

# rename the file and copy to a new directory
for (file in matching_files) {
  new_name <- gsub("MUMM_energy_cables_ETRS89_2", paste0(this_country, file_suffix), basename(file))  # Replace "MUMM" with "BE_4326" in the filename
  destination_path <- file.path(getwd(), "00_data", new_name)  # Construct the destination path
  file.copy(file, destination_path, overwrite = T)
}

## wrangle data ----------------------------------------------------------

BE_SPC <- sf::st_read(file.path(getwd(), "00_data", paste0(this_country, file_suffix, ".shp")))
BE_SPC_orig <- BE_SPC # for comparison
BE_SPC_orig -> BE_SPC

# check CRS
st_crs(BE_SPC)
# transform to CRS
BE_SPC <- st_transform(BE_SPC, this_crs)
# control CRS
st_crs(BE_SPC)

BE_SPC <-
  BE_SPC %>% 
  # rename columns
    dplyr::rename(status = version,
                  comment = network,
                  owner = operator,
                  technical_info = specific_t,
                  id = inspire_id) %>% 
  # set new variables
    dplyr::mutate(country = this_country, 
                  status = ifelse(status == 'As-Built', 'inUse', status)) %>%
  # select relevant columns
    dplyr::select(all_of(these_column_names)) %>% 
  # add missing columns
    add_missing_cols(these_column_names, .)


## write file --------------------------------------------------------------
# sf::st_write(obj = BE_SPC, dsn = file.path(getwd(), "00_data", paste0(this_country, file_suffix, ".shp")), append = F)


# 02. Germany -------------------------------------------------------------

this_country <- "GER" # set country variable to Germany

## get data ----------------------------------------------------------------

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
# GER_request

# read geospatial data
GER_SPC <- sf::st_read(GER_request)

# remove variables
rm(GER_wfs, GER_request, GER_url, GER_client, GER_url, GER_layers)

# check class and CRS
class(GER_SPC)
sf::st_crs(GER_SPC)

# set crs
sf::st_crs(GER_SPC) <- this_crs

# # # write .shp file
# sf::st_write(obj = GER_SPC, dsn = file.path(getwd(), "00_data", paste0("GER", file_suffix)), append = F)


## data wrangling ----------------------------------------------------

# if local file should be read:
# GER_SPC <- sf::st_read(file.path(getwd(), "00_data", paste0(this_country, file_suffix, ".shp")))

GER_SPC_orig <- GER_SPC # for comparison
GER_SPC_orig -> GER_SPC 

# check CRS
sf::st_crs(GER_SPC) # already in EPSG:4326

these_status <- GER_SPC$status %>% unique() # all possible status values 

GER_SPC <- 
  GER_SPC %>% 
    dplyr::rename(id = gml_id,
                  name = name_,
                  technical_info = featuretype_name,
                  geometry = geom) %>%
    # dplyr::filter(status == 'inUse') %>% # only currently used cables
    dplyr::mutate(country = "GER") %>%  # add column with country name
    dplyr::select(any_of(these_column_names)) %>%
    add_missing_cols(these_column_names, .)

## write file --------------------------------------------------------------
# sf::st_write(obj = GER_SPC, dsn = file.path(getwd(), "00_data", paste0(this_country, file_suffix, ".shp")), append = F)

# 03. France --------------------------------------------------------------

# Data Source: https://odre.opendatasoft.com/explore/dataset/lignes-souterraines-rte-nv/map/?disjunctive.etat&disjunctive.tension&location=8,49.38952,-4.33136&basemap=jawg.light
this_country <- "FR"

## open and copy files -----------------------------------------------------

# List all files with the string "lignes-souterraines" in the repository, these are the French submarine power cable shapefiles
matching_files <- list.files(recursive = TRUE, full.names = TRUE, pattern = "lignes-souterraines")
# matching_files

# rename the file and copy to a new directory
for (file in matching_files) {
  new_name <- gsub("lignes-souterraines-rte-nv", paste0("FR", file_suffix), basename(file))  # Replace "MUMM" with "BE_4326" in the filename
  destination_path <- file.path(getwd(), "00_data", new_name)  # Construct the destination path
  file.copy(file, destination_path, overwrite = T)
}

FR_SPC <- sf::st_read(file.path(getwd(), "00_data", paste0(this_country, file_suffix, ".shp")))

## data wrangling ----------------------------------------------------------

# check CRS
st_crs(FR_SPC) # already in EPSG:4326

# all subterrestrial cables need to be removed, thus geospatial data for France as a country is required
FR <- mregions2::gaz_search(17) %>% mregions2::gaz_geometry()
sf::st_crs(FR) # check crs

FR_SPC <- sf::st_difference(FR_SPC, FR) # get only marine SPCs

FR_SPC_orig <- FR_SPC # for comparison
FR_SPC_orig -> FR_SPC

FR_SPC <- 
  FR_SPC %>%
    dplyr::select(1:10) %>% 
    dplyr::rename(id = code_ligne,
                  name = nom_ouvrage,
                  owner = proprietair,
                  status = etat,
                  voltage = tension) %>%
    dplyr::mutate(country = "FR",
                  status = ifelse(status == "EN EXPLOITATION","inUse", 
                                  ifelse(status == "ACCORD ADMINISTRATIF", "Approved", status))) %>% 
    add_missing_cols(these_column_names, .)

## write file --------------------------------------------------------------
# sf::st_write(obj = FR_SPC, dsn = file.path(getwd(), "00_data", paste0(this_country, file_suffix, ".shp")), append = F)

# 04. Netherlands ---------------------------------------------------------
# TO DO: get WFS layers (or sth else that is reproducible)

this_country <- "NL"

## get data WFS (in progress) ----------------------------------------------

# # WFS URL
# NL_wfs <- "https://geo.rijkswaterstaat.nl/services/ogc/gdr/kabels_en_leidingen_noordzee/ows?service%3DWFS"
# 
# # make client
# NL_client <- WFSClient$new(NL_wfs, 
#                            serviceVersion = "2.0.0") # from ows4R
# # get layers
# NL_layers <- NL_client$getFeatureTypes(pretty = TRUE) # from ows4R
# 
# # make http request
# NL_url <- httr::parse_url(NL_wfs) 
# NL_url$query <- list(service = "wfs",
#                      version = "2.0.0", # optional
#                      request = "GetFeature",
#                      typename = NL_layers$name[2])
# NL_request <- httr::build_url(NL_url)
# # NL_request
# 
# # read geospatial data
# NL_SPC <- sf::st_read(NL_request)

## open and copy files -----------------------------------------------------

# List all files with the string "Cobra" or "electra" in the repository, these are part of the Dutch submarine power cable shapefiles
matching_files <- list.files(recursive = TRUE, full.names = TRUE, pattern = "Cobra|electra")
matching_files

# open the two .shp files
NL_1_SPC <- sf::st_read(file.path(getwd(), matching_files[grep("\\.shp$", matching_files)][1]))
NL_2_SPC <- sf::st_read(file.path(getwd(), matching_files[grep("\\.shp$", matching_files)][2]))


## data wrangling --------------------------------------------------------

NL_1_orig <- NL_1_SPC
NL_1_orig -> NL_1_SPC # for comparison

NL_2_orig <- NL_2_SPC
NL_2_orig -> NL_2_SPC

# # transform CRS
# sf::st_crs(NL_1_SPC)
# NL_1_SPC <- sf::st_transform(NL_1_SPC, this_crs)
# sf::st_crs(NL_2_SPC)
# NL_2_SPC <- sf::st_transform(NL_2_SPC, this_crs)

# create standardised columns
NL_1_SPC <- 
  NL_1_SPC %>% 
    dplyr::rename(name = Project,
                  technical_info = Comment) %>%
    dplyr::mutate(country = "NL",
                  status = "inUse") %>% 
    dplyr::select(any_of(these_column_names)) %>%
    add_missing_cols(these_column_names, .)

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
                      ifelse(status == 'Ingebruik', 'inUse',
                             ifelse(status == 'Toekomstig', 'planned', status))) %>%
    dplyr::select(any_of(these_column_names)) %>%
    add_missing_cols(these_column_names, .)

NL_SPC <- rbind(NL_1_SPC, NL_2_SPC)

# transform crs
sf::st_crs(NL_SPC) 
NL_SPC <- sf::st_transform(NL_SPC, this_crs)
rm(NL_1_SPC, NL_1_orig, NL_2_SPC, NL_2_orig)

## write file --------------------------------------------------------------
# sf::st_write(obj = NL_SPC, dsn = file.path(getwd(), "00_data", paste0(this_country, file_suffix, ".shp")), append = F)


# 07. EMODnet Human Activities ------------------------------------------------
# TO DO: either do emodnetwfs or provide download link

this_country <- "NO"

## open and copy files -----------------------------------------------------

# List all files with the string "MUMM" in the repository, these are the Belgian submarine power cable shapefiles
matching_files <- list.files(recursive = TRUE, full.names = TRUE, pattern = "NO")
matching_files

# rename the file and copy to a new directory
for (file in matching_files) {
  new_name <- gsub("NO_NVE_Sjøkabel", paste0(this_country, file_suffix), basename(file))  # Replace filename
  destination_path <- file.path(getwd(), "00_data", new_name)  # Construct the destination path
  file.copy(file, destination_path, overwrite = T)
}

## wrangle data ----------------------------------------------------------

# NO_SPC <- sf::st_read(file.path(getwd(), "00_data", paste0(this_country, file_suffix, ".shp"))) # does not work for some reason
NO_SPC <- st_read(matching_files[6])
NO_SPC_orig <- NO_SPC # for comparison

sf::st_crs(NO_SPC) # already in EPSG:4326

NO_SPC <- 
  NO_SPC %>% 
    dplyr::rename(owner = eier,
                  name = navn,
                  voltage = spenning_k,
                  id = lokalID,
                  comment = eksportTyp,
                  technical_info = objektType) %>% 
    dplyr::mutate(country = "NO",
                status = 'inUse') %>% 
    dplyr::select(any_of(these_column_names)) %>% 
    add_missing_cols(these_column_names, .) %>%
    sf::st_zm(., drop = T, what = "ZM") # convert 'LINESTRING Z' to 'LINESTRING'


# test <-
#   NO_SPC %>%
#     dplyr::mutate(length = sf::st_length(geometry)) #works!

# max(test$length)
# mean(test$length)
# median(test$length)
# min(test$length)

# 08. combine everything --------------------------------------------------

SPC_4326 <- 
  rbind(BE_SPC,
        GER_SPC,
        FR_SPC,
        NL_SPC,
        NO_SPC) %>% 
  dplyr::mutate(length_km = st_length(geometry),
                status = ifelse(status == 'planned', "Planned", status)) %>% 
  dplyr::filter(! status %in% c("OutOfUse", "Unknown", "UnderConstruction"))

units(SPC_4326$length_km) <- 'km'

# st_crs(SPC_4326) # crs still EPSG:4326

SPC_3035 <- st_transform(SPC_4326, "EPSG:3035")
st_crs(SPC_3035)

## write file --------------------------------------------------------------
sf::st_write(obj = SPC_4326, dsn = file.path(getwd(), "02_results", "SPC_4326.shp"), append = F)
sf::st_write(obj = SPC_3035, dsn = file.path(getwd(), "02_results", "SPC_3035.shp"), append = F)


# overview map ------------------------------------------------------------

ggplot() +
  geom_sf(data = SPC)  

leaflet() %>% 
  addTiles() %>% 
  # addPolylines(data = GER_SPC) %>% 
  # addPolylines(data = BE_SPC) %>%
  # addPolylines(data = NL_1_SPC) %>% 
  # addPolylines(data = NL_2_SPC) %>% 
  addPolylines(data = FR_SPC) %>% 
  addPolylines(data = NL_SPC)

leaflet() %>% 
  addTiles() %>% 
  addPolylines(data = NO_SPC,
               opacity = 1) %>%
  addScaleBar(options = scaleBarOptions())

# remove files ------------------------------------------------------------

rm(BE_SPC_orig, GER_SPC_orig, FR_SPC_orig, NO_SPC_orig)
