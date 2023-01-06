#######################################################################################################################################################
#
#
# (c) Copyright Electricity Affordability Dashboard by Lauren Patterson, 2021
# This script obtains electricity service area boundaries
#
#
########################################################################################################################################################

######################################################################################################################################################################
#
#   LOAD LIBRARIES
#
######################################################################################################################################################################
library(rstudioapi)
library(sf);   #libraries to manipulate spatial data
library(leaflet)
library(tidyverse); library(lubridate); #libraries to manipulate data
library(jsonlite); library(httr);   #libraries to obtain data
library(rmapshaper); library(geojsonio); library(curl);  #libraries to minimize and safe out spatial files as geojsons
library(plotly)

options(scipen=999) #changes scientific notation to numeric
rm(list=ls()) #removes anything stored in memory
rm(list = setdiff(ls(), lsf.str())) #removes anything bunt functions
######################################################################################################################################################################

#state lists
state.list <- c("ca", "pa", "nc", "tx", "or");  state.fips <- c("06","42", "37", "48", "41")
selected.year <- 2019;
folder.year <- 2021;

# Set working directory to source file location if code is in similar directory to data files
source_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(source_path))

swd_data <- paste0("..\\data_",folder.year,"\\")
swd_results <- paste0("..\\results_", folder.year,"\\")

`%notin%` = function(x,y) !(x %in% y); #function to get what is not in the list


###################################################################################################################################################################
#
# (1) UPDATE UTILITY SPATIAL BOUNDARIES
#
####################################################################################################################################################################
# Pull service area boundaries 
#data source: https://hifld-geoplatform.opendata.arcgis.com/datasets/c4fd0b01c2544a2f83440dab292f0980_0
#e.sys <- read_sf("https://opendata.arcgis.com/datasets/c4fd0b01c2544a2f83440dab292f0980_0.geojson"); 
e.sys <- read_sf("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Retail_Service_Territories/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
e2.sys <-  e.sys %>% st_transform(crs = 4326) %>% ms_simplify(keep=0.5, keep_shapes=TRUE)
e2.sys <- e2.sys %>% filter(COUNTRY == "USA")
test <- e2.sys %>% filter(STATE=="TX")
mapview::mapview(test)
#geojson_write(e2.sys, file = paste0(swd_data ,"electricity_systems.geojson"))
saveRDS(e2.sys, file = here::here("data","electricity_systems_2022.RDS"))

ngas <- read_sf("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Natural_Gas_Local_Distribution_Company_Service_Territories/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
ngas <- ngas %>% st_transform(crs=4326) %>% ms_simplify(keep = 0.5, keep_shapes = TRUE)
test <- ngas %>% filter(STATE=="TX")
saveRDS(ngas, file = here::here("data","natural_gas_systems_2022.RDS"))
#Note there is a lot of overlap... will need someone to determine how to prioritize or take average value

###################################################################################################################################################################
#
# (2) Update rates data
#
####################################################################################################################################################################
#currently manual becauze saved in gz - I'm sure can be programmed by someone later
#data source: https://openei.org/apps/USURDB/
e.rates <- read.csv(paste0(swd_data, "usurdb.csv"))
length(unique(e.rates$eiaid))



#all.in.out <- ca.sys %>% filter(pwsid==ca.pwsid[1]) %>% mutate(category = "delete") %>% select(pwsid, gis_name, category, geometry)
`%notin%` = function(x,y) !(x %in% y); #function to get what is not in the list

