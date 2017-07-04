library(httr)
library(jsonlite)
options(stringsAsFactors = FALSE)
suppressWarnings(bk <- readLines("beakey.txt"))
key <- paste("?&UserID=", bk, sep = "")
url <- "https://www.bea.gov/api/data"

meth <- "GETDATASETLIST"
form <- "JSON"

call <- paste(url, key, 
              "&method=", meth, 
              "&ResultFormat=", form, 
              sep = "")

raw.result <- GET(url = call)
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
str(this.content)
names(this.content$BEAAPI$Results$Dataset)
this.content$BEAAPI$Results$Dataset



meth <- "getparameterlist"
ds <- "RegionalProduct"
call <- paste(url, key, 
              "&method=", meth,
              "&datasetname=", ds,
              "&ResultFormat=", form, 
              sep = "")

param.RP.raw.result <- GET(url = call)
p.RP.this.raw.content <- rawToChar(param.RP.raw.result$content)
p.RP.this.content <- fromJSON(p.RP.this.raw.content)
str(p.RP.this.content)
names(p.RP.this.content$BEAAPI$Results$Parameter)
p.RP.this.content$BEAAPI$Results$Parameter



meth <- "GetParameterValues"
pn <- "Year"
call <- paste(url, key, 
              "&method=", meth,
              "&datasetname=", ds,
              "&ParameterName=", pn,
              "&ResultFormat=", form, 
              sep = "")

y.RP.raw.result <- GET(url = call)
y.RP.this.raw.content <- rawToChar(y.RP.raw.result$content)
y.RP.this.content <- fromJSON(y.RP.this.raw.content)
str(y.RP.this.content)
min(y.RP.this.content$BEAAPI$Results$ParamValue$Desc)
max(y.RP.this.content$BEAAPI$Results$ParamValue$Desc)



library(dplyr)

pn <- "Component"
call <- paste(url, key, 
              "&method=", meth,
              "&datasetname=", ds,
              "&ParameterName=", pn,
              "&ResultFormat=", form, 
              sep = "")

c.RP.this.content <- GET(url = call)$content %>%
  rawToChar() %>%
  fromJSON(); str(c.RP.this.content)

c.RP.this.content$BEAAPI$Results$ParamValue



pn <- "IndustryId"
call <- paste(url, key, 
              "&method=", meth,
              "&datasetname=", ds,
              "&ParameterName=", pn,
              "&ResultFormat=", form, 
              sep = "")

i.RP.this.content <- GET(url = call)$content %>%
  rawToChar() %>%
  fromJSON(); str(i.RP.this.content)

i.RP.this.content$BEAAPI$Results$ParamValue



getGDP <- function(comp, area = "STATE", inds = "1", year = "ALL"){
  
  suppressWarnings(bk <- readLines("beakey.txt"))
  key <- paste("?&UserID=", bk, sep = "")
  url <- "https://www.bea.gov/api/data"
  
  meth <- "GetData"
  ds <- "RegionalProduct"
  call <- paste(url, key, 
                "&method=", meth,
                "&datasetname=", ds,
                "&Component=", comp,
                "&GeoFips=", area,
                "&IndustryId=", inds,
                "&Year=", year, 
                sep = "")
  
  content <- GET(url = call)$content %>%
    rawToChar() %>%
    fromJSON()
  
  tidyGDP <- content$BEAAPI$Results$Data %>%
    select(TimePeriod, GeoName, Code, CL_UNIT, DataValue) %>%
    rename(year = TimePeriod, 
           area = GeoName, 
           code = Code, 
           currency = CL_UNIT, 
           gdp = DataValue) %>%
    mutate(year = as.integer(year),
           gdp = as.integer(gdp))
  
  return(tidyGDP)
  
}

NAICS <- getGDP(comp = "GDP_SAN")
SIC <- getGDP(comp = "GDP_SAS")
REAL.NAICS <- getGDP(comp = "RGDP_SAN")
REAL.SIC <- getGDP(comp = "RGDP_SAS")
REALpcNAICS <- getGDP(comp = "PCRGDP_SAN")
REALpcSIC <- getGDP(comp = "PCRGDP_SAS")

GDPdata <- rbind(NAICS, SIC, REAL.NAICS, REAL.SIC, REALpcNAICS, REALpcSIC) %>%
  arrange(year, area)



options(stringsAsFactors = TRUE)