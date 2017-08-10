#### Center on Reinventing Public Education #### 
# Title: Denver Lat Lon Cleaning
# Description: Adding Lat and Lon to the Denver datasets (only some were applicable)
# Created by: Kevin Cha on 07-05-17
# Updated by: Kevin Cha on 08-10-17
# Data from: denver_census_clean.R Cleaning
# Link to Github: https://github.com/CRPE-UWB/Local

# Set Up -----------------------------------------------------------------------------
rm(list=ls())

setwd("/Users/crpe/Documents/denver_clean/")

library(dplyr)
library(stringr)

# Function -----------------------------------------------------------------------------
# Adds a lat and lon to the dataset (w/ address and zip)
geocodeAddress <- function(address, zip) {
  require(RJSONIO)
  full <- paste(address, zip, sep=" ")
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, full, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lat,
             x$results[[1]]$geometry$location$lng)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

# Adds a lat and lon to the dataset (w/ address, but no zip)
geocodeAddressLite <- function(address) {
  require(RJSONIO)
  full <- paste(address)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, full, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lat,
             x$results[[1]]$geometry$location$lng)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}


# Affordable Housing Unit (NO FULL_ADDRESS, YES TRACT) -----------------------------------------------------------------------------
# Read in .csv file
affordable_housing <- read.csv("/Users/crpe/Documents/denver_clean/data/affordable_housing_units.csv")
# glimpse at it
glimpse(affordable_housing)
# Make sure the FULL_ADDRESS column is char
affordable_housing$FULL_ADDRESS <- as.character(affordable_housing$FULL_ADDRESS) 

# 1 is longitude
# 2 is latitude
for (i in 1:nrow(affordable_housing)) {
  affordable_housing$LATITUDE[i] <- geocodeAddress(affordable_housing$FULL_ADDRESS[i], affordable_housing$ZIPCODE[i])[1]
  affordable_housing$LONGITUDE[i] <- geocodeAddress(affordable_housing$FULL_ADDRESS[i], affordable_housing$ZIPCODE[i])[2]
}

# Write to .csv file
write.csv(affordable_housing, "clean_data/affordable_housing_c.csv")

# Bike Racks (NO FULL_ADDRESS, NO TRACT) -----------------------------------------------------------------------------
# Read in .csv file
bike <- read.csv("/Users/crpe/Documents/denver_clean/data/bike_racks.csv")
# glimpse at it
glimpse(bike)


# Crime (Y; already has it) -----------------------------------------------------------------------------
# Read in .csv file
crime <- read.csv("/Users/crpe/Documents/denver_clean/data/crime.csv", stringsAsFactors = FALSE)
backup_crime <- crime
crime <- backup_crime
# glimpse at it
glimpse(crime)
# Make sure the FULL_ADDRESS column is char
crime$INCIDENT_ADDRESS <- as.character(crime$INCIDENT_ADDRESS) 

# separate the data column into specifically for year, month, day, time
crime$FIRST_OCCURRENCE_YEAR <- substring(crime$FIRST_OCCURRENCE_DATE, 1, 4)
crime$FIRST_OCCURRENCE_MONTH <- substring(crime$FIRST_OCCURRENCE_DATE, 6, 7)
crime$FIRST_OCCURRENCE_DAY <- substring(crime$FIRST_OCCURRENCE_DATE, 9, 10)
crime$FIRST_OCCURRENCE_TIME <- substring(crime$FIRST_OCCURRENCE_DATE, 12, 20)
crime$LAST_OCCURRENCE_YEAR <- substring(crime$LAST_OCCURRENCE_DATE, 1, 4)
crime$LAST_OCCURRENCE_MONTH <- substring(crime$LAST_OCCURRENCE_DATE, 6, 7)
crime$LAST_OCCURRENCE_DAY <- substring(crime$LAST_OCCURRENCE_DATE, 9, 10)
crime$LAST_OCCURRENCE_TIME <- substring(crime$LAST_OCCURRENCE_DATE, 12, 20)
crime$REPORTED_YEAR <- substring(crime$REPORTED_DATE, 1, 4)
crime$REPORTED_MONTH <- substring(crime$REPORTED_DATE, 6, 7)
crime$REPORTED_DAY <- substring(crime$REPORTED_DATE, 9, 10)
crime$REPORTED_TIME <- substring(crime$REPORTED_DATE, 12, 20)

# change col order
crime <- setcolorder(crime,c("INCIDENT_ID", "OFFENSE_ID", "OFFENSE_CODE", "OFFENSE_CODE_EXTENSION", "OFFENSE_TYPE_ID", "OFFENSE_CATEGORY_ID",
                          "FIRST_OCCURRENCE_DATE", "FIRST_OCCURRENCE_YEAR", "FIRST_OCCURRENCE_MONTH", "FIRST_OCCURRENCE_DAY", "FIRST_OCCURRENCE_TIME",
                          "LAST_OCCURRENCE_DATE", "LAST_OCCURRENCE_YEAR", "LAST_OCCURRENCE_MONTH", "LAST_OCCURRENCE_DAY", "LAST_OCCURRENCE_TIME",
                          "REPORTED_DATE", "REPORTED_YEAR", "REPORTED_MONTH", "REPORTED_DAY", "REPORTED_TIME",
                          "INCIDENT_ADDRESS", "GEO_X", "GEO_Y", "GEO_LON", "GEO_LAT", "DISTRICT_ID", "PRECINCT_ID", "NEIGHBORHOOD_ID", "IS_CRIME", "IS_TRAFFIC"))

# turn some empty columns to NAs/-99
crime$INCIDENT_ADDRESS[crime$INCIDENT_ADDRESS == ""] <- -99

# ARRANGE BY FIRST_OCCURENCE_YEAR 
crime <- crime %>%  arrange(desc(FIRST_OCCURRENCE_DATE))

# FOR Simon 
crime2 <- crime
crime2$LAST_OCCURRENCE_DATE <- NULL
crime2$LAST_OCCURRENCE_YEAR <- NULL
crime2$LAST_OCCURRENCE_MONTH <- NULL
crime2$LAST_OCCURRENCE_DAY <- NULL
crime2$LAST_OCCURRENCE_TIME <- NULL
crime2$REPORTED_DATE <- NULL
crime2$REPORTED_YEAR <- NULL
crime2$REPORTED_MONTH <- NULL
crime2$REPORTED_DAY <- NULL
crime2$REPORTED_TIME <- NULL

crime2 <- crime2 %>% 
            select(!contains("traf")) %>% 
            filter(OFFENSE_TYPE_ID)


# Write to .csv fileS
write.csv(crime, "clean_data/crime_c.csv")
write.csv(crime2, "clean_data/crime_s.csv")

# Fire Stations (Y) -----------------------------------------------------------------------------
# Read in .csv file
fire <- read.csv("/Users/crpe/Documents/denver_clean/data/fire_stations.csv")
# glimpse at it
glimpse(fire)
# Make sure the FULL_ADDRESS column is char
fire$FULL_ADDRESS <- as.character(fire$FULL_ADDRESS) 

# 1 is longitude
# 2 is latitude
for (i in 1:nrow(fire)) {
  fire$LATITUDE[i] <- geocodeAddress(fire$FULL_ADDRESS[i], fire$ZIPCODE[i])[1]
  fire$LONGITUDE[i] <- geocodeAddress(fire$FULL_ADDRESS[i], fire$ZIPCODE[i])[2]
}

# Write to .csv file
write.csv(fire, "clean_data/fire_stations_c.csv")

# Food Stores (Y; already has POINTX+POINTY (aka LON+LAT)) -----------------------------------------------------------------------------
# Read in .csv file
food <- read.csv("/Users/crpe/Documents/denver_clean/data/food_stores.csv")
# glimpse at it
glimpse(food)
# Make sure the ADDRESS_LINE1 column is char
food$ADDRESS_LINE1 <- as.character(food$ADDRESS_LINE1) 

# 1 is longitude
# 2 is latitude
for (i in 1:nrow(food)) {
  food$LATITUDE[i] <- geocodeAddress(food$ADDRESS_LINE1[i], food$ZIP[i])[1]
  food$LONGITUDE[i] <- geocodeAddress(food$ADDRESS_LINE1[i], food$ZIP[i])[2]
}

# Write to .csv file
write.csv(food, "clean_data/food_stores_c.csv")

# Head Start Administrative Offices (Y) -----------------------------------------------------------------------------
# Read in .csv file
head_start <- read.csv("/Users/crpe/Documents/denver_clean/data/head_start_administrative_offices.csv")
# glimpse at it
glimpse(head_start)
# Make sure the FULL_ADDRESS column is char
head_start$ADDRESS_LINE1 <- as.character(head_start$ADDRESS_LINE1) 

# 1 is longitude
# 2 is latitude
for (i in 1:nrow(head_start)) {
  head_start$LATITUDE[i] <- geocodeAddress(head_start$ADDRESS_LINE1[i], head_start$ZIPCODE[i])[1]
  head_start$LONGITUDE[i] <- geocodeAddress(head_start$ADDRESS_LINE1[i], head_start$ZIPCODE[i])[2]
}

# Write to .csv file
write.csv(head_start, "clean_data/head_start_administrative_offices_c.csv")

# Historic District Contributing Structures (NO ZIP) -----------------------------------------------------------------------------
# Read in .csv file
historic <- read.csv("/Users/crpe/Documents/denver_clean/data/historic_district_contributing_structures.csv")
# glimpse at it
glimpse(historic)
# Make sure the ADDRESS_LINE1 column is char
historic$ADDRESS_LINE1 <- as.character(historic$ADDRESS_LINE1) 

# 1 is longitude
# 2 is latitude
for (i in 1:nrow(historic)) {
  historic$LATITUDE[i] <- geocodeAddressLite(historic$ADDRESS_LINE1[i])[1]
  historic$LONGITUDE[i] <- geocodeAddressLite(historic$ADDRESS_LINE1[i])[2]
}

# Write to .csv file
write.csv(historic, "clean_data/historic_district_contributing_structures_c.csv")

# Libraries (Y) -----------------------------------------------------------------------------
# Read in .csv file
library <- read.csv("/Users/crpe/Documents/denver_clean/data/libraries.csv")
# glimpse at it
glimpse(library)
# Make sure the ADDRESS_LINE1 column is char
library$ADDRESS_LINE1 <- as.character(library$ADDRESS_LINE1) 

# 1 is longitude
# 2 is latitude
for (i in 1:nrow(library)) {
  library$LATITUDE[i] <- geocodeAddress(library$ADDRESS_LINE1[i], library$ZIP[i])[1]
  library$LONGITUDE[i] <- geocodeAddress(library$ADDRESS_LINE1[i], library$ZIP[i])[2]
}

# Write to .csv file
write.csv(library, "clean_data/libraries_c.csv")

# Neighborhood Business Revitalization Corridors (N) -----------------------------------------------------------------------------
# Read in .csv file
neigh_biz <- read.csv("/Users/crpe/Documents/denver_clean/data/neighborhood_business_revitalization_corridors.csv")
# glimpse at it
glimpse(neigh_biz)
# Make sure the ADDRESS_LINE1 column is char
neigh_biz$ADDRESS_LINE1 <- as.character(neigh_biz$ADDRESS_LINE1) 

# 1 is longitude
# 2 is latitude
for (i in 1:nrow(neigh_biz)) {
  neigh_biz$LATITUDE[i] <- geocodeAddress(neigh_biz$ADDRESS_LINE1[i], neigh_biz$ZIP[i])[1]
  neigh_biz$LONGITUDE[i] <- geocodeAddress(neigh_biz$ADDRESS_LINE1[i], neigh_biz$ZIP[i])[2]
}

# Write to .csv file
write.csv(neigh_biz, "clean_data/neighborhood_business_revitalization_corridors_c.csv")

# Parks (Y; already has it) -----------------------------------------------------------------------------
# Read in .csv file
park <- read.csv("/Users/crpe/Documents/denver_clean/data/parks.csv")
# glimpse at it
glimpse(park)


# Pedestrian Routes(N) -----------------------------------------------------------------------------
# Read in .csv file
ped <- read.csv("/Users/crpe/Documents/denver_clean/data/pedestrian_routes.csv")
# glimpse at it
glimpse(ped)


# Police Stations (Y) -----------------------------------------------------------------------------
# Read in .csv file
police <- read.csv("/Users/crpe/Documents/denver_clean/data/police_stations.csv")
# glimpse at it
glimpse(police)
# Make sure the FULL_ADDRESS column is char
police$ADDRESS_LINE1 <- as.character(police$ADDRESS_LINE1) 

# 1 is longitude
# 2 is latitude
for (i in 1:nrow(police)) {
  police$LATITUDE[i] <- geocodeAddress(police$ADDRESS_LINE1[i], police$ZIP[i])[1]
  police$LONGITUDE[i] <- geocodeAddress(police$ADDRESS_LINE1[i], police$ZIP[i])[2]
}

# Write to .csv file
write.csv(police, "clean_data/police_stations_c.csv")

# Recreation Centers (Y; already has it) -----------------------------------------------------------------------------
# Read in .csv file
rec_center <- read.csv("/Users/crpe/Documents/denver_clean/data/recreation_centers.csv")
# glimpse at it
glimpse(rec_center)


# Registered Neighborhood Organizations (Y) -----------------------------------------------------------------------------
# Read in .csv file
reg_neigh <- read.csv("/Users/crpe/Documents/denver_clean/data/registered_neighborhood_organizations.csv")
# glimpse at it
glimpse(reg_neigh)
# Make sure the FULL_ADDRESS column is char
reg_neigh$ADDRESS <- as.character(reg_neigh$ADDRESS) 

# 1 is longitude
# 2 is latitude
for (i in 1:nrow(reg_neigh)) {
  reg_neigh$LATITUDE[i] <- geocodeAddress(reg_neigh$ADDRESS[i], reg_neigh$ZIP[i])[1]
  reg_neigh$LONGITUDE[i] <- geocodeAddress(reg_neigh$ADDRESS[i], reg_neigh$ZIP[i])[2]
}

# Write to .csv file
write.csv(reg_neigh, "clean_data/registered_neighborhood_organizations_c.csv")

# Sheriff Office Locations (Y) -----------------------------------------------------------------------------
# Read in .csv file
sheriff <- read.csv("/Users/crpe/Documents/denver_clean/data/sheriffs_office_locations.csv")
# glimpse at it
glimpse(sheriff)
# Make sure the FULL_ADDRESS column is char
sheriff$ADDRESS_LINE1 <- as.character(sheriff$ADDRESS_LINE1) 

# 1 is longitude
# 2 is latitude
for (i in 1:nrow(sheriff)) {
  sheriff$LATITUDE[i] <- geocodeAddress(sheriff$ADDRESS_LINE1[i], sheriff$ZIP[i])[1]
  sheriff$LONGITUDE[i] <- geocodeAddress(sheriff$ADDRESS_LINE1[i], sheriff$ZIP[i])[2]
}

# Write to .csv file
write.csv(sheriff, "clean_data/sheriff_office_locations_c.csv")

# Traffic Accidents (Y; ALREADY HAS IT) -----------------------------------------------------------------------------
# Read in .csv file
traffic <- read.csv("/Users/crpe/Documents/denver_clean/data/traffic_accidents.csv")
# glimpse at it
glimpse(traffic)


# Trails and Sidewalks (N) -----------------------------------------------------------------------------
# Read in .csv file
trails <- read.csv("/Users/crpe/Documents/denver_clean/data/trails_and_sidewalks.csv")
# glimpse at it
glimpse(trails)
# Make sure the FULL_ADDRESS column is char
trails$FULL_ADDRESS <- as.character(trails$FULL_ADDRESS) 

# 1 is longitude
# 2 is latitude
for (i in 1:nrow(trails)) {
  trails$LATITUDE[i] <- geocodeAddress(trails$FULL_ADDRESS[i], trails$ZIPCODE[i])[1]
  trails$LONGITUDE[i] <- geocodeAddress(trails$FULL_ADDRESS[i], trails$ZIPCODE[i])[2]
}

# Write to .csv file
write.csv(trails, "clean_data/trails_and_sidewalks_c.csv")
