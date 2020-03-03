library(tidycensus)
library(tidyverse)
library(tigris)
library(stringr)
library(sf)
library(lazyeval)
library(RPostgreSQL)

geos <- c("state","county","tract","bg","place")
state_codes <- unique(fips_codes$state_code)[1:51]
#ctys <- counties(cb = TRUE)
#states <- c("al", "ak", "az", "ar", "ca", "co", "ct", "de", "dc", "fl", "ga", "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me", "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", "nj", "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "pr", "ri", "sc", "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi", "wy")
vars2018 <- load_variables(2018, "acs5") #get the table names
#trim unwanted table names, positions vary year to year
# vars2011 <- vars2011[-c(42079:42101),] #remove extras at the bottom
# vars2011 <- vars2011[-c(39732),] #remove BLKGRP
# vars2011 <- vars2011[-c(1),] #remove extras at the top
# vars2017 <- vars2017[-c(1,2,3,4),] #remove extras at the top
# vars2017 <- vars2017[-c(25072:25106),] #remove extras at the bottom
# vars2017 <- vars2017[-c(23349),] #remove BLKGRP
vars2018 <- vars2018[-c(24829:25274),]
tablevector <- vars2018$name #create a vector of the table names
tablevector <- sub("E$","",tablevector) #remove last Es
tablevector <- sub("M$","",tablevector) #remove last Ms
tablevector <- unique(tablevector)

pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user="postgres", password="egcdcatbhcab", host="104.197.26.248", port=5433, dbname="acs1418")

tempv <- ""
statelist <- vector()
countylist <- vector()
placelist <- vector()
tractlist <- vector()
bglist <- vector()

for (tv in tablevector){
  #tv <- word(tv,1,sep = fixed('_'))
  if (word(tv,1,sep = fixed('_')) != tempv){
    tempv <- word(tv,1,sep = fixed('_'))
    #do comparison between that table and postgres
    temp <- dbReadTable(con, c('data',tolower(tempv)))
    temp <- temp[order(temp$geonum),]
    
    #State
    statesdo <- temp[7,2]
    getacs <- get_acs(geography = "state", state = "08", variable = tv, year = 2018)
    stateacs <- getacs[1,4]
    if (is.na(statesdo)){
      print(paste("State NA",tv))
    } else if (statesdo == stateacs){
      print(paste("State Matched",tv))
    } else {
      statelist <- c(statelist, tempv)
    }
    
    #County
    countysdo <- temp[298,2]
    getacs <- get_acs(geography = "county", state = "08", variable = tv, year = 2018)
    countyacs <- getacs[1,4]
    if (is.na(countysdo)|is.na(countyacs)){
      print(paste("County NA",tv))
    } else if (countysdo == countyacs){
      print(paste("County Matched",tv))
    } else {
      countylist <- c(countylist, tempv)
    }

    # #Place
    # placesdo <- temp[6811,2]
    # getacs <- get_acs(geography = "place", state = "08", variable = tv, year = 2018)
    # placeacs <- getacs[88,4]
    # if (is.na(placesdo)|is.na(placeacs)){
    #   print(paste("Place NA",tv))
    # } else if (placesdo == placeacs){
    #   print(paste("Place Matched",tv))
    # } else {
    #   placelist <- c(placelist, tempv)
    # }
    # 
    # #Tracts
    # tractsdo <- temp[44464,2]
    # getacs <- get_acs(geography = "tract", state = "08", variable = tv, year = 2018)
    # tractacs <- getacs[1,4]
    # if (is.na(tractsdo)|is.na(tractacs)){
    #   print(paste("Tract NA",tv))
    # } else if (tractsdo == tractacs){
    #   print(paste("Tract Matched",tv))
    # } else {
    #   tractlist <- c(tractlist, tempv)
    # }
    
  # #Block groups
  #   bgsdo <- temp[8,2]
  #   getacs <- get_acs(geography = "block group", state = "08", variable = tv, year = 2018)
  #   bgacs <- getacs[1,4]
  #   if (is.na(bgsdo)){
  #   } else if (bgsdo == bgacs){
  #     print("Matched")
  #   } else {
  #     bglist <- c(bglist, tempv)
  #   }

  }
}
  

#acstab <- get_acs(geography = geotype, variables = tv500, year = 2017, output = "wide")