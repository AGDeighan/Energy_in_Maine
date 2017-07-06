library(httr)
library(jsonlite)
library(dplyr)

suppressWarnings(ek <- readLines("EIAkey.txt"))
key <- paste("api_key=", ek, sep = ""); rm(ek)
uribase <- "http://api.eia.gov/"

meth <- "series/?"
sabb <- "AK"
srID <- paste("&series_id=", "SEDS.TETCB.", sabb, ".A", sep = "")

call <- paste(uribase,
              meth,
              key,
              srID,
              sep = "")

raw.result <- GET(url = call); raw.result$status_code

this.content <- raw.result$content %>% 
  rawToChar() %>%
  fromJSON()

consumption <- as.data.frame(this.content$series$data) %>%
  rename(year = X1, bill.btu = X2) %>%
  mutate(state = sabb,
         year = as.integer(as.character(year)), 
         bill.btu = as.integer(as.character(bill.btu))) %>%
  select(state, year, bill.btu) %>%
  filter(year >= 1980)

################################################################################

totalconsumption <- function(state) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  
  suppressWarnings(ek <- readLines("EIAkey.txt"))
  key <- paste("api_key=", ek, sep = ""); rm(ek)
  uribase <- "http://api.eia.gov/"
 
  meth <- "series/?"
  sabb <- state
  srID <- paste("&series_id=", "SEDS.TETCB.", sabb, ".A", sep = "")
  
  call <- paste(uribase,
                meth,
                key,
                srID,
                sep = "")
  
  raw.result <- GET(url = call); raw.result$status_code
  
  this.content <- raw.result$content %>% 
    rawToChar() %>%
    fromJSON()
  
  consumption <- as.data.frame(this.content$series$data) %>%
    rename(year = X1, bill.btu = X2) %>%
    mutate(state = sabb,
           sector = "All",
           source = "All",
           year = as.integer(as.character(year)), 
           bill.btu = as.integer(as.character(bill.btu))) %>%
    select(state, sector, source, year, bill.btu) %>%
    filter(year >= 1980) 
  
  return(consumption)
  rm(key, uribase, meth, sabb, srID, call, raw.result, this.content, consumption)
}

total.consumption.data <- data.frame(state = NULL,
                                     sector = NULL,
                                     source = NULL,
                                     year = NULL,
                                     bill.btu = NULL)

states <- c(state.abb, "DC")
for(sname in states){
  total.consumption.data <- rbind(total.consumption.data, 
                                  totalconsumption(sname))
}

################################################################################

t.elctr.consumption <- function(state) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  
  suppressWarnings(ek <- readLines("EIAkey.txt"))
  key <- paste("api_key=", ek, sep = ""); rm(ek)
  uribase <- "http://api.eia.gov/"
  
  meth <- "series/?"
  sabb <- state
  srID <- paste("&series_id=", "SEDS.TEEIB.", sabb, ".A", sep = "")
  
  call <- paste(uribase,
                meth,
                key,
                srID,
                sep = "")
  
  raw.result <- GET(url = call); raw.result$status_code
  
  this.content <- raw.result$content %>% 
    rawToChar() %>%
    fromJSON()
  
  consumption <- as.data.frame(this.content$series$data) %>%
    rename(year = X1, bill.btu = X2) %>%
    mutate(state = sabb,
           sector = "Electric Power",
           source = "All",
           year = as.integer(as.character(year)), 
           bill.btu = as.integer(as.character(bill.btu))) %>%
    select(state, sector, source, year, bill.btu) %>%
    filter(year >= 1980) 
  
  return(consumption)
  rm(key, uribase, meth, sabb, srID, call, raw.result, this.content, consumption)
}

elctr.consumption.data <- data.frame(state = NULL,
                                     sector = NULL,
                                     source = NULL,
                                     year = NULL,
                                     bill.btu = NULL)

states <- c(state.abb, "DC")
for(sname in states){
  elctr.consumption.data <- rbind(elctr.consumption.data, 
                                  t.elctr.consumption(sname))
}

################################################################################

t.resid.consumption <- function(state) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  
  suppressWarnings(ek <- readLines("EIAkey.txt"))
  key <- paste("api_key=", ek, sep = ""); rm(ek)
  uribase <- "http://api.eia.gov/"
  
  meth <- "series/?"
  sabb <- state
  srID <- paste("&series_id=", "SEDS.TERCB.", sabb, ".A", sep = "")
  
  call <- paste(uribase,
                meth,
                key,
                srID,
                sep = "")
  
  raw.result <- GET(url = call); raw.result$status_code
  
  this.content <- raw.result$content %>% 
    rawToChar() %>%
    fromJSON()
  
  consumption <- as.data.frame(this.content$series$data) %>%
    rename(year = X1, bill.btu = X2) %>%
    mutate(state = sabb,
           sector = "Residential",
           source = "All",
           year = as.integer(as.character(year)), 
           bill.btu = as.integer(as.character(bill.btu))) %>%
    select(state, sector, source, year, bill.btu) %>%
    filter(year >= 1980) 
  
  return(consumption)
  rm(key, uribase, meth, sabb, srID, call, raw.result, this.content, consumption)
}

resid.consumption.data <- data.frame(state = NULL,
                                     sector = NULL,
                                     source = NULL,
                                     year = NULL,
                                     bill.btu = NULL)

states <- c(state.abb, "DC")
for(sname in states){
  resid.consumption.data <- rbind(resid.consumption.data, 
                                  t.resid.consumption(sname))
}

################################################################################

t.cmrcl.consumption <- function(state) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  
  suppressWarnings(ek <- readLines("EIAkey.txt"))
  key <- paste("api_key=", ek, sep = ""); rm(ek)
  uribase <- "http://api.eia.gov/"
  
  meth <- "series/?"
  sabb <- state
  srID <- paste("&series_id=", "SEDS.TECCB.", sabb, ".A", sep = "")
  
  call <- paste(uribase,
                meth,
                key,
                srID,
                sep = "")
  
  raw.result <- GET(url = call); raw.result$status_code
  
  this.content <- raw.result$content %>% 
    rawToChar() %>%
    fromJSON()
  
  consumption <- as.data.frame(this.content$series$data) %>%
    rename(year = X1, bill.btu = X2) %>%
    mutate(state = sabb,
           sector = "Commercial",
           source = "All",
           year = as.integer(as.character(year)), 
           bill.btu = as.integer(as.character(bill.btu))) %>%
    select(state, sector, source, year, bill.btu) %>%
    filter(year >= 1980) 
  
  return(consumption)
  rm(key, uribase, meth, sabb, srID, call, raw.result, this.content, consumption)
}

cmrcl.consumption.data <- data.frame(state = NULL,
                                     sector = NULL,
                                     source = NULL,
                                     year = NULL,
                                     bill.btu = NULL)

states <- c(state.abb, "DC")
for(sname in states){
  cmrcl.consumption.data <- rbind(cmrcl.consumption.data, 
                                  t.cmrcl.consumption(sname))
}

################################################################################


t.indus.consumption <- function(state) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  
  suppressWarnings(ek <- readLines("EIAkey.txt"))
  key <- paste("api_key=", ek, sep = ""); rm(ek)
  uribase <- "http://api.eia.gov/"
  
  meth <- "series/?"
  sabb <- state
  srID <- paste("&series_id=", "SEDS.TEICB.", sabb, ".A", sep = "")
  
  call <- paste(uribase,
                meth,
                key,
                srID,
                sep = "")
  
  raw.result <- GET(url = call); raw.result$status_code
  
  this.content <- raw.result$content %>% 
    rawToChar() %>%
    fromJSON()
  
  consumption <- as.data.frame(this.content$series$data) %>%
    rename(year = X1, bill.btu = X2) %>%
    mutate(state = sabb,
           sector = "Industrial",
           source = "All",
           year = as.integer(as.character(year)), 
           bill.btu = as.integer(as.character(bill.btu))) %>%
    select(state, sector, source, year, bill.btu) %>%
    filter(year >= 1980) 
  
  return(consumption)
  rm(key, uribase, meth, sabb, srID, call, raw.result, this.content, consumption)
}

indus.consumption.data <- data.frame(state = NULL,
                                     sector = NULL,
                                     source = NULL,
                                     year = NULL,
                                     bill.btu = NULL)

states <- c(state.abb, "DC")
for(sname in states){
  indus.consumption.data <- rbind(indus.consumption.data, 
                                  t.indus.consumption(sname))
}

################################################################################

t.trans.consumption <- function(state) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  
  suppressWarnings(ek <- readLines("EIAkey.txt"))
  key <- paste("api_key=", ek, sep = ""); rm(ek)
  uribase <- "http://api.eia.gov/"
  
  meth <- "series/?"
  sabb <- state
  srID <- paste("&series_id=", "SEDS.TEACB.", sabb, ".A", sep = "")
  
  call <- paste(uribase,
                meth,
                key,
                srID,
                sep = "")
  
  raw.result <- GET(url = call); raw.result$status_code
  
  this.content <- raw.result$content %>% 
    rawToChar() %>%
    fromJSON()
  
  consumption <- as.data.frame(this.content$series$data) %>%
    rename(year = X1, bill.btu = X2) %>%
    mutate(state = sabb,
           sector = "Transportation",
           source = "All",
           year = as.integer(as.character(year)), 
           bill.btu = as.integer(as.character(bill.btu))) %>%
    select(state, sector, source, year, bill.btu) %>%
    filter(year >= 1980) 
  
  return(consumption)
  rm(key, uribase, meth, sabb, srID, call, raw.result, this.content, consumption)
}

trans.consumption.data <- data.frame(state = NULL,
                                     sector = NULL,
                                     source = NULL,
                                     year = NULL,
                                     bill.btu = NULL)

states <- c(state.abb, "DC")
for(sname in states){
  trans.consumption.data <- rbind(trans.consumption.data, 
                                  t.trans.consumption(sname))
}

################################################################################

consumptiondata <- rbind(resid.consumption.data,
                         cmrcl.consumption.data,
                         indus.consumption.data,
                         trans.consumption.data,
                         elctr.consumption.data,
                         total.consumption.data)

write.csv(consumption, "data/Energy Consumption Data.csv", row.names = FALSE)