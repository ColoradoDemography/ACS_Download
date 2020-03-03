#devtools::install_github("ColoradoDemography/tidycensus")
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
#vars2017 <- load_variables(2017, "acs5") #get the table names
vars2018 <- load_variables(2018, "acs5")
#trim unwanted table names, positions vary year to year
# vars2011 <- vars2011[-c(42079:42101),] #remove extras at the bottom
# vars2011 <- vars2011[-c(39732),] #remove BLKGRP
# vars2011 <- vars2011[-c(1),] #remove extras at the top
#vars2017 <- vars2017[-c(1,2,3,4),] #remove extras at the top
#vars2017 <- vars2017[-c(25072:25106),] #remove extras at the bottom
#vars2017 <- vars2017[-c(23349),] #remove BLKGRP
vars2018 <- vars2018[-c(24829:25274),]
tablevector <- vars2018$name #create a vector of the table names
tablevector <- sub("E$","",tablevector) #remove last Es
tablevector <- sub("M$","",tablevector) #remove last Ms
tablevector <- unique(tablevector) #ditch the duplicates

# acstab <- get_acs(geography = "us", variables = c("B00001_001","B00002_001"), year = 2011, output = "wide")
# acstab <- rbind(acstab, get_acs(geography = "state", variables = "B00001_001", endyear = 2010, output = "wide"))
# acstab <- rbind(acstab, get_acs(geography = "county", variables = "B00001_001", endyear = 2010, output = "wide"))
# acstab <- rbind(acstab, get_acs(geography = "place", variables = "B00001_001", endyear = 2010, output = "wide"))
# acstab <- rbind(acstab, get_acs(geography = "tract", state = state_codes, variables = "B00001_001", endyear = 2010, output = "wide"))

# Run the following two functions as a whole to be used later
# get chunks of 500 to speed things up
acs500 <- function(geotype){
  x = 1
  for (i in 1:length(tablevector)){
    if ((i %% 500) == 0 | i == length(tablevector)){
      tv500 = tablevector[x:i]
      if (x == 1){
        acstab <- get_acs(geography = geotype, variables = tv500, year = 2018, output = "wide")
      }
      else{
        acstab <- cbind(acstab, get_acs(geography = geotype, variables = tv500, year = 2018, output = "wide"))
      }
      print(paste(x,i))
      x = x + 500
    }
  }
  acstab <- acstab[, !duplicated(colnames(acstab))]
  return(acstab)
}

acs500tract <- function(tract_state){
  x = 1
  for (i in 1:length(tablevector)){
    if ((i %% 500) == 0 | i == length(tablevector)){
      tv500 = tablevector[x:i]
      if (x == 1){
        acstab <- get_acs(geography = "tract", state = tract_state, variables = tv500, year = 2018, output = "wide")
      } #change geography to tract or block group depending
      else{
        acstab <- cbind(acstab, get_acs(geography = "tract", state = tract_state, variables = tv500, year = 2018, output = "wide"))
      } #change geography to tract or block group depending
      print(paste(x,i))
      x = x + 500
    }
  }
  acstab <- acstab[, !duplicated(colnames(acstab))]
  acstab <- acstab[, order(names(acstab))]
  return(acstab)
}

#stateshort is for a partial list, change to state_codes for all states. Manual for block groups.
for (st in state_codes){
  nam <- paste0("tract",st) #change to tract or bg depending
  print(nam)
  assign(nam, acs500tract(st))
  write.csv(get(nam), file = paste0(nam,".csv"))
  print(paste0("Clearing ",nam))
  assign(nam,NULL)
}

#Get Block Group Data - Not working well for 2010, will try again for 2011

#extract fields that have block group values
bgvector = c()
nullvector = c()
for (tv in tablevector){
  tryCatch({message(paste("Trying",tv))
    get_acs(geography = "block group", variables = tv, state = "08", county = "035", year = "2018")
    bgvector = c(bgvector,tv)
  },
  error=function(cond){
    #message(paste(tv,"doesn't exist for bgs"))
    message(cond)
    nullvector = c(nullvector,tv)
  })
}

#convert bgvector.txt (stored result of above) into a vector
bgcolumns = read.delim("bgvector.txt",header = FALSE,sep = "\n")
bgvector = as.vector(bgcolumns$V1)

acs500bg <- function(bg_county){
    # bgtab = NULL
    # cnty_list = ctys[ctys$STATEFP == bg_state,]
    # for (bg_county in cnty_list$COUNTYFP){
      x = 1
      print(paste("Running",bg_county,"in",bg_state))
      for (i in 1:length(bgvector)){
        if ((i %% 100) == 0 | i == length(bgvector)){
          tv500 = bgvector[x:i]
          if (x == 1){
            acstab <- get_acs(geography = "block group", state = bg_state, county = bg_county, variables = tv500, year = 2011, output = "wide")
          }
          else{
            acstab <- cbind(acstab, get_acs(geography = "block group", state = bg_state, county = bg_county, variables = tv500, year = 2011, output = "wide"))
          }
          print(paste(x,i))
          x = x + 100
        }
      }
    # acstab <- acstab[, !duplicated(colnames(acstab))]
    # if (state == "AL"){
    #   acstabbg <- acstab
    # }
    # else{
    #   acstabbg <- cbind(acstabbg,acstab)
  #}
    #bgtab = rbind(bgtab,acstab)
  #}
  acstab <- acstab[, !duplicated(colnames(acstab))]
  acstab <- acstab[, order(names(acstab))]    
  return(acstab)
}

# for (bg_state in stateshort){
#   #cnty_list = ctys[ctys$STATEFP == bg_state,]
#   nam <- paste0("bg",bg_state)
#   print(nam)
#   assign(nam, acs500bg(bg_state))
#   write.csv(get(nam), file = paste0(nam,".csv"))
#   print(paste0("Clearing ",nam))
#   assign(nam,NULL)
# }





#Table Manipulation to add us and states
#getdata <- function(file)
#states <- read.csv(file="states.csv", header=TRUE, sep=",", colClasses = "character")
#states <- rbind(acstabus, acstabstate) #cbind the results of acs500("us") and acs500("states")

#When appending, use states <- acs500()
states <- states[, order(names(states))]
#Split data and margin of error
statedata <- select_(states, lazyeval::interp(~ends_with(x), x = "E"))
statedata$NAM <- NULL
statemoe <- select_(states, lazyeval::interp(~ends_with(x), x = "M"))

#Rename fields
names(statedata) <- substr(names(statedata),1,nchar(names(statedata))-1)
names(statedata) <- sub("_","",names(statedata))
names(statemoe) <- substr(names(statemoe),1,nchar(names(statemoe))-1)
names(statemoe) <- sub("_","_MOE",names(statemoe))

#Add name and geoid columns
statedata <-cbind(GEONUM=as.character(states$GEOID),statedata)
statedata <-cbind(NAME=states$NAME,statedata)
statemoe <-cbind(GEONUM=as.character(states$GEOID),statemoe)
statemoe <-cbind(NAME=states$NAME,statemoe)

#Change geoid to geonum
statedata$GEONUM <- sub("^","1",statedata$GEONUM)
statemoe$GEONUM <- sub("^","1",statemoe$GEONUM)

#Brute force the US Geonum (only use for US level data)
#statedata[1,2] <- "30"
#statemoe[1,2] <- "30"

#make all columns numeric
statedata <- sapply(statedata, as.numeric) #drop Name and Nam in production
statemoe <- sapply(statemoe, as.numeric)

#Connect to Postgresql
pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user="postgres", password="egcdcatbhcab", host="104.197.26.248", port=5433, dbname="acs1418")

testcolumns <- colnames(statemoe) #change to statemoe to load moe

#do the loop and write tables with states data to database
tabname <- ""
collist <- c()
for (column in testcolumns){
  if (column == "NAME" | column == "GEONUM" | column == "NAM"){}
  else{
    colstart <- substr(column, start = 1, stop = nchar(column)-3)
    #print(colstart)
    if (colstart != tabname){
      if (is.null(collist) == FALSE){
        dbtable <- as.data.frame(subset(statemoe,select=c("GEONUM",collist))) #change to statemoe to load moe
        names(dbtable) <- tolower(names(dbtable))
        dbWriteTable(con,c('data',tolower(tabname)),dbtable,row.names=FALSE)
      }
      #print(collist)
      collist <- c(column)
      tabname <- colstart
    }
    else {
      collist <- c(collist,column)
    }
  }
}

#append to already written tables
#to add individual tables, run the script below, then run the script within the innermost if again
testcolumns <- colnames(statedata) #change to statemoe to load moe
temptable <- "temptable"
tabname <- ""
collist <- c()
for (column in testcolumns){
  if (column == "NAME" | column == "GEONUM" | column == "NAM"){}
  else{
    colstart <- substr(column, start = 1, stop = nchar(column)-3)
    print(colstart)
    if (colstart != tabname){
      if (is.null(collist) == FALSE){
        dbtable <- as.data.frame(subset(statedata,select=c("GEONUM",collist))) #change to moe if necessary
        #dbtable <- as.data.frame(sapply(dbtable, as.numeric)) Only necessary when as.numeric above fails 
        names(dbtable) <- tolower(names(dbtable))
        dbWriteTable(con,c('data',temptable),dbtable,row.names=FALSE)
        sql <- paste0("INSERT INTO data.", tolower(tabname)," SELECT * FROM data.", temptable,";" )
        dbSendQuery(con, sql)
        dropsql <- paste0("DROP TABLE data.", temptable)
        dbSendQuery(con,dropsql)
      }
      print(collist)
      collist <- c(column)
      tabname <- colstart
    }
    else {
      collist <- c(collist,column)
    }
  }
}

#function for looping
getdata <- function(file,type){
  states <- read.csv(file=file, header=TRUE, sep=",", colClasses = "character")

  states <- states[, order(names(states))]
  #Split data and margin of error
  statedata <- select_(states, lazyeval::interp(~ends_with(x), x = "E"))
  statedata$NAM <- NULL
  statemoe <- select_(states, lazyeval::interp(~ends_with(x), x = "M"))
  
  #Rename fields
  names(statedata) <- substr(names(statedata),1,nchar(names(statedata))-1)
  names(statedata) <- sub("_","",names(statedata))
  names(statemoe) <- substr(names(statemoe),1,nchar(names(statemoe))-1)
  names(statemoe) <- sub("_","_MOE",names(statemoe))
  
  #Add name and geoid columns
  statedata <-cbind(GEONUM=as.character(states$GEOID),statedata)
  statedata <-cbind(NAME=states$NAME,statedata)
  statemoe <-cbind(GEONUM=as.character(states$GEOID),statemoe)
  statemoe <-cbind(NAME=states$NAME,statemoe)
  
  #Change geoid to geonum
  statedata$GEONUM <- sub("^","1",statedata$GEONUM)
  statemoe$GEONUM <- sub("^","1",statemoe$GEONUM)
  
  
  #make all columns numeric
  statedata <- sapply(statedata, as.numeric) #drop Name and Nam in production
  statemoe <- sapply(statemoe, as.numeric)
  
  ifelse(type == "data", return(statedata),return(statemoe))

}

#Connect to Postgresql
pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user="postgres", password="egcdcatbhcab", host="104.197.26.248", port=5433, dbname="acs1418")
temp = list.files(pattern="*.csv")

#loop for tract files
for (tractfile in temp){ 
  print(paste0(tractfile, " has begun"))
  statedata = getdata(tractfile,"data") #change data to moe for the margins of error
  testcolumns <- colnames(statedata) #Run twice, change statedata to statemoe
  temptable <- "temptable"
  tabname <- ""
  collist <- c()

  for (column in testcolumns){
    if (column == "NAME" | column == "GEONUM" | column == "NAM"){}
    else{
      colstart <- substr(column, start = 1, stop = nchar(column)-3)
      #print(colstart)
      if (colstart != tabname){
        if (is.null(collist) == FALSE){
          dbtable <- as.data.frame(subset(statedata,select=c("GEONUM",collist))) #And change this to moe
          #dbtable <- as.data.frame(sapply(dbtable, as.numeric)) Only necessary when as.numeric above fails 
          names(dbtable) <- tolower(names(dbtable))
          dbWriteTable(con,c('data',temptable),dbtable,row.names=FALSE)
          sql <- paste0("INSERT INTO data.", tolower(tabname)," SELECT * FROM data.", temptable,";" )
          dbSendQuery(con, sql)
          dropsql <- paste0("DROP TABLE data.", temptable)
          dbSendQuery(con,dropsql)
        }
        #print(collist)
        collist <- c(column)
        tabname <- colstart
      }
      else {
        collist <- c(collist,column)
      }
    }
  }
  print(paste0(tractfile," is complete"))
}

#place loading loop
#x <- c("001","002","003","004","005","006","007","008","009")
#for (val in x){
temp = list.files(pattern="*.csv")

#loop for tract files
for (tractfile in temp){ 
  print(paste0(tractfile, " has begun"))
  states <- read.csv((file=tractfile), header=TRUE, sep=",", colClasses = "character")
  #overnight loop for places
  states <- states[, order(names(states))]
  statedata <- select_(states, lazyeval::interp(~ends_with(x), x = "E"))
  statedata$NAM <- NULL
  statemoe <- select_(states, lazyeval::interp(~ends_with(x), x = "M"))
  
  #Rename fields
  names(statedata) <- substr(names(statedata),1,nchar(names(statedata))-1)
  names(statedata) <- sub("_","",names(statedata))
  names(statemoe) <- substr(names(statemoe),1,nchar(names(statemoe))-1)
  names(statemoe) <- sub("_","_MOE",names(statemoe))
  
  #Add name and geoid columns
  statedata <-cbind(GEONUM=as.character(states$GEOID),statedata)
  statedata <-cbind(NAME=states$NAME,statedata)
  statemoe <-cbind(GEONUM=as.character(states$GEOID),statemoe)
  statemoe <-cbind(NAME=states$NAME,statemoe)
  
  #Change geoid to geonum
  statedata$GEONUM <- sub("^","1",statedata$GEONUM)
  statemoe$GEONUM <- sub("^","1",statemoe$GEONUM)
  
  #make all columns numeric
  statedata <- sapply(statedata, as.numeric) #drop Name and Nam in production
  statemoe <- sapply(statemoe, as.numeric)
  
  testcolumns <- colnames(statedata) #change to statemoe to load moe
  temptable <- "temptable"
  tabname <- ""
  collist <- c()
  for (column in testcolumns){
    if (column == "NAME" | column == "GEONUM" | column == "NAM"){}
    else{
      colstart <- substr(column, start = 1, stop = nchar(column)-3)
      #print(colstart)
      if (colstart != tabname){
        if (is.null(collist) == FALSE){
          dbtable <- as.data.frame(subset(statedata,select=c("GEONUM",collist))) #change to moe if necessary
          #dbtable <- as.data.frame(sapply(dbtable, as.numeric)) Only necessary when as.numeric above fails 
          names(dbtable) <- tolower(names(dbtable))
          dbWriteTable(con,c('data',temptable),dbtable,row.names=FALSE)
          sql <- paste0("INSERT INTO data.", tolower(tabname)," SELECT * FROM data.", temptable,";" )
          dbSendQuery(con, sql)
          dropsql <- paste0("DROP TABLE data.", temptable)
          dbSendQuery(con,dropsql)
        }
        #print(collist)
        collist <- c(column)
        tabname <- colstart
      }
      else {
        collist <- c(collist,column)
      }
    }
  }
  
  testcolumns <- colnames(statemoe) #change to statemoe to load moe
  temptable <- "temptable"
  tabname <- ""
  collist <- c()
  for (column in testcolumns){
    if (column == "NAME" | column == "GEONUM" | column == "NAM"){}
    else{
      colstart <- substr(column, start = 1, stop = nchar(column)-3)
      #print(colstart)
      if (colstart != tabname){
        if (is.null(collist) == FALSE){
          dbtable <- as.data.frame(subset(statemoe,select=c("GEONUM",collist))) #change to moe if necessary
          #dbtable <- as.data.frame(sapply(dbtable, as.numeric)) Only necessary when as.numeric above fails 
          names(dbtable) <- tolower(names(dbtable))
          dbWriteTable(con,c('data',temptable),dbtable,row.names=FALSE)
          sql <- paste0("INSERT INTO data.", tolower(tabname)," SELECT * FROM data.", temptable,";" )
          dbSendQuery(con, sql)
          dropsql <- paste0("DROP TABLE data.", temptable)
          dbSendQuery(con,dropsql)
        }
        #print(collist)
        collist <- c(column)
        tabname <- colstart
      }
      else {
        collist <- c(collist,column)
      }
    }
  }
  print(tractfile)
}

#loop to run block groups by county in colorado
counties = c("001","003","005","007","009","011","013","014","015","017","019","021","023","025","027","029","031","033","035","037","039","041","043","045","047","049","051","053","055","057","059","061","063","065","067","069","071","073","075","077","079","081","083","085","087","089","091","093","095","097","099","101","103","105","107","109","111","113","115","117","119","121","123","125")
for (cnty in counties){
  nam <- paste0("bg08",cnty)
  assign(nam, acs500bg(cnty))
  write.csv(get(nam), file = paste0(nam,".csv"))
  print(paste0("Clearing ",nam))
  assign(nam,NULL)
}

for (st in state_codes){
  temptab <- get_acs(geography = "block group", state = st, variable = "B19013_001", year = 2017, output = "wide")
  acstab <- rbind(acstab, temptab)
  print(paste0("State ",st))
}

#run all tracts...run first county then the rest
for (st in state_codes2){
  tract_data <- acs500tract(st)
  states <- rbind(states, tract_data)
}