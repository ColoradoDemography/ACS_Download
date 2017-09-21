library(tidycensus)
library(tidyverse)
library(tigris)
library(stringr)
library(sf)

geos <- c("state","county","tract","bg","place")
state_codes <- unique(fips_codes$state_code)[1:51]
ctys <- counties(cb = TRUE)
#states <- c("al", "ak", "az", "ar", "ca", "co", "ct", "de", "dc", "fl", "ga", "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me", "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", "nj", "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "pr", "ri", "sc", "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi", "wy")
vars2010 <- load_variables(2010, "acs5") #get the table names
vars2010 <- vars2010[-c(1,2,3,4,5),] #remove extras at the top
vars2010 <- vars2010[-c(41860:41893),] #remove extras at the bottom
vars2010 <- vars2010[-c(39305),] #remove BLKGRP
tablevector <- vars2010$name #create a vector of the table names
tablevector <- sub("E$","",tablevector) #remove last Es
tablevector <- sub("M$","",tablevector) #remove last Ms
tablevector <- unique(tablevector) #ditch the duplicates

acstab <- get_acs(geography = "us", variables = c("B01001_001","B01001_002"), endyear = 2010, output = "wide")
acstab <- rbind(acstab, get_acs(geography = "state", variables = "B00001_001", endyear = 2010, output = "wide"))
acstab <- rbind(acstab, get_acs(geography = "county", variables = "B00001_001", endyear = 2010, output = "wide"))
acstab <- rbind(acstab, get_acs(geography = "place", variables = "B00001_001", endyear = 2010, output = "wide"))
acstab <- rbind(acstab, get_acs(geography = "tract", state = state_codes, variables = "B00001_001", endyear = 2010, output = "wide"))

#get chunks of 500 to speed things up
acs500 <- function(geotype){
system.time({
  x = 1
  for (i in 1:length(tablevector)){
    if ((i %% 500) == 0){
      tv500 = tablevector[x:i]
      if (x == 1){
        acstab <- get_acs(geography = geotype, variables = tv500, endyear = 2010, output = "wide")
      }
      else{
        acstab <- cbind(acstab, get_acs(geography = geotype, variables = tv500, endyear = 2010, output = "wide"))
      }
      print(paste(x,i))
      x = x + 500
}}})
}  

acstab <- acstab[, !duplicated(colnames(acstab))] #get rid of duplicate columns

# bgs <- map_df(state_codes, function(state_code) {
#   state <- filter(ctys, STATEFP == state_code)
#   county_codes <- state$COUNTYFP
#   rbind(acstab, get_acs(geography = "block group", variables = "B00001_001",
#           state = state_code, county = county_codes))
# })


for (state in state_codes){
  print(state)
  cnty_list = ctys[ctys$STATEFP == state,]
  acstab <- rbind(acstab, get_acs(geography = "block group", variables = "B00001_001", state = state, county = cnty_list$COUNTYFP, output = "wide"))
}

for (tv in tablevector){
  tryCatch({  
    get_acs(geography = "us", variables = tv, endyear = 2010, output = "wide")
  }, error=function(e){cat("ERROR :",conditionMessage(e),tv,"\n")})
}
