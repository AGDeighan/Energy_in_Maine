
## State CO2 data sets were all downloaded manually on June 29th from https://www.eia.gov/environment/emissions/state/

library(xlsx)
library(dplyr)

# Initialize data frame
plot(1980, 100, type = "n", xlim = c(1980, 2015), ylim = c(10, 250), xlab = "year", ylab = "CO2")
CO2tidy <- data.frame(year = c(),
                      state = c(),
                      sector = c(),
                      source = c(),
                      carbdiox_millmtrcton = c())

# Create vector of filenames
messyfiles <- list.files("data/messy")

# Loop through all files and create tidy data set
i  = 0
for(f in messyfiles){
  i <- i + 1
    # Import data set
  messydata <- read.xlsx(paste("data/messy/", f, sep =""), sheetIndex = 1, header = TRUE)
  # Save state name
  sname <- strsplit(f, "\\.")[[1]][1]
  # Create years, sectors, and sources vectors
  years <- 1980:2014
  sectors <- c(as.character(unique(messydata[4:29,1])[!is.na(unique(messydata[4:29,1]))]), "All")
  sources <- c("Coal", "Petroleum Products", "Natural Gas", "All")
  
  # Create temporary dataframe
  temp <- data.frame(year = rep(years, each = 24),
                     state = rep(sname, 840),
                     sector = rep(rep(sectors, each = 4), length(years)),
                     source = rep(sources, 6*length(years)),
                     carbdiox_millmtrcton = rep(NA, 24*length(years)))
  # create vectors containing row and column indices of CO2 emissions
  r <- c(5:8,11:14,17:20,23:26,29:32,37:40)
  c <- 3:37
  # use row and column indices to add CO2 emissions values
  temp <- temp %>% mutate(carbdiox_millmtrcton = as.vector(as.numeric(as.matrix(messydata[r,c]))))
  h <- filter(temp, sector == "All" & source == "All")
  lines(h$year, h$carbdiox_millmtrcton, col = i)
  # Append temporary dataframe to final data frame
  CO2tidy <- rbind(CO2tidy, temp)
  
}

## Write to csv
#!!!!write.csv(CO2tidy, "data/CO2 Emissions - tidyAll.csv", row.names = FALSE)

library(readr)
#!!!!CO2tidy <- read_csv("data/CO2 Emissions - tidyAll.csv")

## Add region and division column to data frame

CO2tidy$region <- NA
CO2tidy$division <- NA

# create vectors of states within regions and divisions
d1 <- c("connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont")
d2 <- c("new jersey", "new york", "pennsylvania")
r1 <- c(d1, d2)
d3 <- c("illinois", "indiana", "michigan", "ohio", "wisconsin")
d4 <- c("iowa", "kansas", "minnesota", "missouri", "nebraska", "north dakota", "south dakota")
r2 <- c(d3, d4)
d5 <- c("delaware", "florida", "georgia", "maryland", "north carolina", "south carolina", "virginia", "district of columbia", "west virginia")
d6 <- c("alabama", "kentucky", "mississippi", "tennessee")
d7 <- c("arkansas", "louisiana", "oklahoma", "texas")
r3 <- c(d5, d6, d7)
d8 <- c("arizona", "colorado", "idaho", "montana", "nevada", "new mexico", "utah", "wyoming")
d9 <- c("alaska", "california", "hawaii", "oregon", "washington")
r4 <- c(d8, d9)

divisions <- c("new england", "mid-atlantic", 
               "east north central", "west north central", 
               "south atlantic", "east south cenral", "west south central", 
               "mountain", "pacific")

CO2tidy$division[CO2tidy$state %in% d1] <- divisions[1]
CO2tidy$division[CO2tidy$state %in% d2] <- divisions[2]
CO2tidy$division[CO2tidy$state %in% d3] <- divisions[3]
CO2tidy$division[CO2tidy$state %in% d4] <- divisions[4]
CO2tidy$division[CO2tidy$state %in% d5] <- divisions[5]
CO2tidy$division[CO2tidy$state %in% d6] <- divisions[6]
CO2tidy$division[CO2tidy$state %in% d7] <- divisions[7]
CO2tidy$division[CO2tidy$state %in% d8] <- divisions[8]
CO2tidy$division[CO2tidy$state %in% d9] <- divisions[9]

regions <- c("northeast", "midwest", "south", "west")

CO2tidy$region[CO2tidy$state %in% r1] <- regions[1]
CO2tidy$region[CO2tidy$state %in% r2] <- regions[2]
CO2tidy$region[CO2tidy$state %in% r3] <- regions[3]
CO2tidy$region[CO2tidy$state %in% r4] <- regions[4]

# Convert regions and divisions to factors
CO2tidy$region <- factor(CO2tidy$region, levels = regions)
CO2tidy$division <- factor(CO2tidy$division, levels = divisions)


## Now we will load the population data, this datefile was downloaded manually 
## on June 29th 2017 from https://seer.cancer.gov/popdata/download.html. The
## data set is the single-year age groups set for all states combined (adjusted).
## We have to import the file in sections because it is so small

popmessy <- read_fwf("data/us.1969_2015.singleages.adjusted.txt",
                     fwf_positions(c(1,5,19),
                                   c(4,6,26),
                                   c("year", "state", "population")),
                     skip = 11000000,
                     n_max = 10000000
                    )

popmessy$population <- as.integer(popmessy$population)

small <- group_by(popmessy, year, state) %>% summarise(population = sum(population))

rm(popmessy)

poptidy <- small

popmessy <- read_fwf("data/us.1969_2015.singleages.adjusted.txt",
                     fwf_positions(c(1,5,19),
                                   c(4,6,26),
                                   c("year", "state", "population")),
                     skip = 21000000,
                     n_max = 10000000
)

popmessy$population <- as.integer(popmessy$population)

small <- group_by(popmessy, year, state) %>% summarise(population = sum(population))

rm(popmessy)

poptidy <- rbind(poptidy, small)

popmessy <- read_fwf("data/us.1969_2015.singleages.adjusted.txt",
                     fwf_positions(c(1,5,19),
                                   c(4,6,26),
                                   c("year", "state", "population")),
                     skip = 31000000,
                     n_max = 10000000
)

popmessy$population <- as.integer(popmessy$population)

small <- group_by(popmessy, year, state) %>% summarise(population = sum(population))

rm(popmessy)

poptidy <- rbind(poptidy, small)

popmessy <- read_fwf("data/us.1969_2015.singleages.adjusted.txt",
                     fwf_positions(c(1,5,19),
                                   c(4,6,26),
                                   c("year", "state", "population")),
                     skip = 41000000,
                     n_max = 10000000
)

popmessy$population <- as.integer(popmessy$population)

small <- group_by(popmessy, year, state) %>% summarise(population = sum(population))

rm(popmessy)

poptidy <- rbind(poptidy, small)

popmessy <- read_fwf("data/us.1969_2015.singleages.adjusted.txt",
                     fwf_positions(c(1,5,19),
                                   c(4,6,26),
                                   c("year", "state", "population")),
                     skip = 51000000,
                     n_max = 10000000
)

popmessy$population <- as.integer(popmessy$population)

small <- group_by(popmessy, year, state) %>% summarise(population = sum(population))

rm(popmessy)

poptidy <- rbind(poptidy, small)

rm(small)

poptidy <- filter(poptidy, year >= 1980) %>%
            ungroup() %>%
            group_by(year, state) %>% 
            summarise(population = sum(population)) %>%
            ungroup()



snames <- tolower(state.name)
for(r in 1:nrow(poptidy)){
    if(length(snames[state.abb == poptidy$state[r]]) > 0){
    poptidy$state[r] <- snames[state.abb == poptidy$state[r]]
    }
}

poptidy$state[poptidy$state == "DC"] <- "district of columbia"

CO2tidy <- merge(CO2tidy, poptidy, by = c("year", "state"), all.x = TRUE)

CO2tidy <- CO2tidy %>% mutate(percapita.co2 = (carbdiox_millmtrcton/population)*1000000)

rm(poptidy)

#!!!!write.csv(CO2tidy, "data/All CO2 Emissions - tidy.csv", row.names = FALSE)

## GDP data was downloaded manually on June 29th 2017 from https://www.bea.gov/regional/downloadzip.cfm
## the data is nominal GDP data (not adjusted for inflation) using SIC method. In 1997 the bureau of
## economic analysis switched to the NAICS method.

gdpmessySIC <- read_csv("data/gsp_sic_all_C.csv", n_max = 4680)

gdpmessySIC <- gdpmessySIC %>% 
  filter(Description == "All industry total") %>% 
  select(-c(GeoFIPS, Region, ComponentId, IndustryId, IndustryClassification))

library(tidyr)

gdptidySIC <- gdpmessySIC %>% gather(year, gdp.nominal, -GeoName, - ComponentName, -Description)

rm(gdpmessySIC)

gdptidySIC <- gdptidySIC %>% 
  select(year, GeoName, gdp.nominal) %>%
  rename(state = GeoName)

snames <- c(tolower(state.name), "district of columbia")

library(blscrapeR)
df <- inflation_adjust(1995)
tail(df)

gdptidySIC <- gdptidySIC %>%
  mutate(year = as.integer(year), 
         state = tolower(state), 
         gdp.nominal = as.integer(gdp.nominal),
         gdp.adjusted = as.integer(as.integer(gdp.nominal) * (inflation_adjust(gdptidySIC$year[1]) %>% tail(1))) ) %>%
  filter(state %in% snames, year >= 1980)

write.csv(gdptidySIC, "data/SIC GDP tidy (from C).csv", row.names = FALSE)

emissions <- tbl_df(merge(CO2tidy, gdptidySIC, by = c("year", "state"), all.x = TRUE))

USemissions <- emissions %>%
  group_by(year, sector, source) %>%
  summarise(population = sum(population),
            carbdiox_millmtrcton = sum(carbdiox_millmtrcton), 
            percapita.co2 = carbdiox_millmtrcton/population * 1000000,
            gdp.nominal = sum(gdp.nominal),
            gdp.adjusted = sum(gdp.adjusted)) %>%
  ungroup()




USemissions <- USemissions %>% mutate(state = "all", 
                                      sector = tolower(sector), 
                                      source = tolower(source),
                                      region = "all",
                                      division = "all",
                                      population = population,
                                      percapita.gdp.adj = gdp.adjusted/population)

emissions <- emissions %>% mutate(sector = tolower(sector), 
                                  source = tolower(source),
                                  percapita.gdp.adj = gdp.adjusted/population)

emissions <- emissions %>% select(year, state, region, division, sector, source, population, carbdiox_millmtrcton, percapita.co2, gdp.nominal, gdp.adjusted, percapita.gdp.adj)
USemissions <- USemissions %>% select(year, state, region, division, sector, source, population, carbdiox_millmtrcton, percapita.co2, gdp.nominal, gdp.adjusted, percapita.gdp.adj)


emissions <- rbind(emissions, USemissions)

write.csv(emissions, "data/CO2 and GDP.csv", row.names = FALSE)