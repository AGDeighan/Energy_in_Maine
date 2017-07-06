## Master data set

################################################################################

# Starting with CO2 emissions data, since that data set includes the most
# catagories as of July 7th (multiple entries for fuel souce)

# load necessary libraries
library(readr)
library(dplyr)

# import emissions and consumption data
emissions <- read_csv("data/CO2 Emissions Data.csv")
consumption <- read_csv("data/Energy Consumption Data.csv")

# merge emissions and consumption data
master <- merge(emissions, consumption, 
                by = c("state", "year", "sector", "source"),
                all.x =  TRUE)

rm(emissions, consumption)

################################################################################

# import gdp data
gdp <- read_csv("data/GDP Data.csv")

# rename "area" column to "state" and convert to lowercase so that the data set
# can easily be merged with the master data set. Use only current dollar (not
# adjust for inflation) gdp values and only observations from 1980 onwards
gdp <- gdp %>%
     rename(state = area) %>%
     mutate(state = tolower(state)) %>%
     filter(currency == "millions of current dollars", year >= 1980)

# use SIC calculated GDP for years before 1997
gdp.sic <- gdp %>%
     filter(year < 1997, code == "GDP_SAS-1")

# use NAICS calculated GDP for years from 1997 onwards
gdp.naics <- gdp %>%
     filter(year >= 1997, code == "GDP_SAN-1")

# rowbind SIC and NAICS gdp data sets
gdp <- rbind(gdp.sic, gdp.naics) %>%
     rename(gdp.mill.curr = gdp) %>%
     select(state, year, gdp.mill.curr)
rm(gdp.sic, gdp.naics)

# merge gdp data into master data set
master <- merge(master, gdp,
                by = c("state", "year"),
                all.x = TRUE)
rm(gdp)

################################################################################

# import population data
population <- read_csv("data/Population Data.csv")

# merge population data with master data set
master <- merge(master, population,
                by = c("state", "year"),
                all.x = TRUE)
rm(population)
################################################################################

## organize master data set geographically

# add region and division columns to data frame

master$region <- NA
master$division <- NA

# create vectors of states within regions and divisions
d1 <- c("connecticut", "maine", "massachusetts", 
        "new hampshire", "rhode island", "vermont")
d2 <- c("new jersey", "new york", "pennsylvania")
r1 <- c(d1, d2)
d3 <- c("illinois", "indiana", "michigan", 
        "ohio", "wisconsin")
d4 <- c("iowa", "kansas", "minnesota", 
        "missouri", "nebraska", "north dakota", 
        "south dakota")
r2 <- c(d3, d4)
d5 <- c("delaware", "florida", "georgia", 
        "maryland", "north carolina", "south carolina", 
        "virginia", "district of columbia", "west virginia")
d6 <- c("alabama", "kentucky", "mississippi", 
        "tennessee")
d7 <- c("arkansas", "louisiana", "oklahoma", 
        "texas")
r3 <- c(d5, d6, d7)
d8 <- c("arizona", "colorado", "idaho", 
        "montana", "nevada", "new mexico", 
        "utah", "wyoming")
d9 <- c("alaska", "california", "hawaii", 
        "oregon", "washington")
r4 <- c(d8, d9)

divisions <- c("new england", "mid-atlantic", "east north central", 
               "west north central", "south atlantic", "east south cenral", 
               "west south central", "mountain", "pacific")

master$division[master$state %in% d1] <- divisions[1]
master$division[master$state %in% d2] <- divisions[2]
master$division[master$state %in% d3] <- divisions[3]
master$division[master$state %in% d4] <- divisions[4]
master$division[master$state %in% d5] <- divisions[5]
master$division[master$state %in% d6] <- divisions[6]
master$division[master$state %in% d7] <- divisions[7]
master$division[master$state %in% d8] <- divisions[8]
master$division[master$state %in% d9] <- divisions[9]

regions <- c("northeast", "midwest", "south", "west")

master$region[master$state %in% r1] <- regions[1]
master$region[master$state %in% r2] <- regions[2]
master$region[master$state %in% r3] <- regions[3]
master$region[master$state %in% r4] <- regions[4]

# Convert regions and divisions to factors
master$region <- factor(master$region, levels = regions)
master$division <- factor(master$division, levels = divisions)

################################################################################

regional <- master %>%
     filter(source == "All") %>%
     group_by(year, region, sector) %>%
     summarise(carbdiox_millmtrcton = sum(carbdiox_millmtrcton),
               bill.btu = sum(bill.btu),
               gdp.mill.curr = sum(gdp.mill.curr),
               population = sum(population)) %>%
     ungroup()

plot(1980, 1,
     xlim = c(1980, 2014),
     ylim = c(0, 0.30),
     type = "n")

i = 0
pe.regions <- list(northeast = NULL, 
                   midwest = NULL,
                   south = NULL,
                   west = NULL)
for(r in unique(regional$region)){
     i <- i + 1
     t <- regional %>%
          filter(sector == "All", region == r)
     pe <- t$gdp.mill.curr/ t$bill.btu
     pe.regions[[r]] <- pe
     yr <- t$year
     lines(yr, pe, col = i)
}




country <- regional %>%
     group_by(year, sector) %>%
     summarise(carbdiox_millmtrcton = sum(carbdiox_millmtrcton),
               bill.btu = sum(bill.btu),
               gdp.mill.curr = sum(gdp.mill.curr),
               population = sum(population)) %>%
     ungroup()

t <- country %>%
     filter(sector == "All")
pe <- t$gdp.mill.curr/ t$bill.btu
pe.us <- pe
yr <- t$year
i <- i + 1
lines(yr, pe, col = (i))

maine <- master %>%
     filter(state == "maine")

t <- maine %>%
     filter(sector == "All", source == "All")

pe <- t$gdp.mill.curr/ t$bill.btu
pe.maine <- pe
yr <- t$year
i <- i + 1
lines(yr, pe, col = (i))

plot(yr, pe.maine/pe.regions$northeast, type = "l",
     xlim = c(1980, 2014), ylim = c(0.4, 1.2), col = 2)
lines(yr, pe.maine/pe.regions$midwest, col = 3)
lines(yr, pe.maine/pe.regions$south, col = 4)
lines(yr, pe.maine/pe.regions$west, col = 5)
lines(yr, pe.maine/pe.us, col = 6)
abline(h = 1)
#legend("topleft",
#       legend = c("northeast", "midwest", "south", "west", "country"),
#       col = c(2:6),
#       lty = 1)


rm(t, i, pe)


write.csv(master, "data/Master Data Set.csv", row.names = FALSE)
