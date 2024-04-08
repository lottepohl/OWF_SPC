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

# 01. Belgium -------------------------------------------------------------------
# Data Source: https://spatial.naturalsciences.be/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?3&filter=false
# Name: od_nature:MUMM_energy_cables_ETRS89_2


# 02. Germany -------------------------------------------------------------


