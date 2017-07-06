## Exploratory analysis

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

master <- read_csv("data/Master Data Set.csv")

regional <- master %>%
     filter(source == "All") %>%
     group_by(year, region, sector) %>%
     summarise(carbdiox_millmtrcton = sum(carbdiox_millmtrcton),
               bill.btu = sum(bill.btu),
               gdp.mill.curr = sum(gdp.mill.curr),
               population = sum(population)) %>%
     ungroup() %>%
     rename(area = region) %>%
     select(year, area, sector, carbdiox_millmtrcton, 
            bill.btu, gdp.mill.curr, population) %>%
     mutate(conv.eff = gdp.mill.curr/bill.btu,
            extrn.eff = bill.btu/carbdiox_millmtrcton,
            emmis.gdp.eff = gdp.mill.curr/carbdiox_millmtrcton)

country <- regional %>%
     group_by(year, sector) %>%
     summarise(carbdiox_millmtrcton = sum(carbdiox_millmtrcton),
               bill.btu = sum(bill.btu),
               gdp.mill.curr = sum(gdp.mill.curr),
               population = sum(population)) %>%
     ungroup() %>%
     mutate(area = "united states") %>%
     select(year, area, sector, carbdiox_millmtrcton, 
            bill.btu, gdp.mill.curr, population) %>%
     mutate(conv.eff = gdp.mill.curr/bill.btu,
            extrn.eff = bill.btu/carbdiox_millmtrcton,
            emmis.gdp.eff = gdp.mill.curr/carbdiox_millmtrcton)

maine <- master %>%
     filter(state == "maine", source == "All") %>%
     rename(area = state) %>%
     select(year, area, sector, carbdiox_millmtrcton, 
            bill.btu, gdp.mill.curr, population) %>%
     mutate(conv.eff = gdp.mill.curr/bill.btu,
            extrn.eff = bill.btu/carbdiox_millmtrcton,
            emmis.gdp.eff = gdp.mill.curr/carbdiox_millmtrcton)

working <- rbind(maine, regional, country)

################################################################################
################## Conversion Efficiency #######################################

comp.conv.eff <- maine %>%
     mutate(me.v.ne = 
                 conv.eff/regional$conv.eff[regional$area == "northeast"],
            me.v.mw = 
                 conv.eff/regional$conv.eff[regional$area == "midwest"],
            me.v.s = 
                 conv.eff/regional$conv.eff[regional$area == "south"],
            me.v.w = 
                 conv.eff/regional$conv.eff[regional$area == "west"],
            me.v.us =
                 conv.eff/country$conv.eff,
            metric = "Conversion Efficiency") %>%
     select(year, metric, sector, me.v.us, me.v.ne, me.v.mw, me.v.s, me.v.w) %>%
     gather(comparison, ce.ratio, me.v.us:me.v.w)


ce.plots.all <- ggplot(filter(comp.conv.eff, sector == "All"),
                       aes(year, ce.ratio)) + 
     geom_line() + 
     facet_wrap(~comparison) +
     labs(title = "All Sectors"); ce.plots.all

ce.plots.cmrcl <- ggplot(filter(comp.conv.eff, sector == "Commercial"),
                       aes(year, ce.ratio)) + 
     geom_line() + 
     facet_wrap(~comparison) +
     labs(title = "Commercial Sector"); ce.plots.cmrcl

ce.plots.elctrc <- ggplot(filter(comp.conv.eff, sector == "Electric Power"),
                       aes(year, ce.ratio)) + 
     geom_line() + 
     facet_wrap(~comparison) +
     labs(title = "Electric Power Sector"); ce.plots.elctrc

ce.plots.indus <- ggplot(filter(comp.conv.eff, sector == "Industrial"),
                       aes(year, ce.ratio)) + 
     geom_line() + 
     facet_wrap(~comparison) +
     labs(title = "Industrial Sector"); ce.plots.indus

ce.plots.resid <- ggplot(filter(comp.conv.eff, sector == "Residential"),
                       aes(year, ce.ratio)) + 
     geom_line() + 
     facet_wrap(~comparison) +
     labs(title = "Residential Sector"); ce.plots.resid

ce.plots.trans <- ggplot(filter(comp.conv.eff, sector == "Transportation"),
                       aes(year, ce.ratio)) + 
     geom_line() + 
     facet_wrap(~comparison) +
     labs(title = "Transportation Sector"); ce.plots.trans

################################################################################
############### Externality Efficiency #########################################

comp.extrn.eff <- maine %>%
     mutate(me.v.ne = 
                 extrn.eff/regional$extrn.eff[regional$area == "northeast"],
            me.v.mw = 
                 extrn.eff/regional$extrn.eff[regional$area == "midwest"],
            me.v.s = 
                 extrn.eff/regional$extrn.eff[regional$area == "south"],
            me.v.w = 
                 extrn.eff/regional$extrn.eff[regional$area == "west"],
            me.v.us =
                 extrn.eff/country$extrn.eff,
            metric = "Externality Efficiency") %>%
     select(year, metric, sector, me.v.us, me.v.ne, me.v.mw, me.v.s, me.v.w) %>%
     gather(comparison, ratio, me.v.us:me.v.w)

ee.plots.all <- ggplot(filter(comp.extrn.eff, sector == "All"),
                       aes(year, ratio)) + 
     geom_line() + 
     facet_wrap(~comparison) +
     labs(title = "All Sectors"); ee.plots.all

ee.plots.cmrcl <- ggplot(filter(comp.extrn.eff, sector == "Commercial"),
                         aes(year, ratio)) + 
     geom_line() + 
     facet_wrap(~comparison) +
     labs(title = "Commercial Sector"); ee.plots.cmrcl

ee.plots.elctrc <- ggplot(filter(comp.extrn.eff, sector == "Electric Power"),
                          aes(year, ratio)) + 
     geom_line() + 
     facet_wrap(~comparison) +
     labs(title = "Electric Power Sector"); ee.plots.elctrc

ee.plots.indus <- ggplot(filter(comp.extrn.eff, sector == "Industrial"),
                         aes(year, ratio)) + 
     geom_line() + 
     facet_wrap(~comparison) +
     labs(title = "Industrial Sector"); ee.plots.indus

ee.plots.resid <- ggplot(filter(comp.extrn.eff, sector == "Residential"),
                         aes(year, ratio)) + 
     geom_line() + 
     facet_wrap(~comparison) +
     labs(title = "Residential Sector"); ee.plots.resid

ee.plots.trans <- ggplot(filter(comp.extrn.eff, sector == "Transportation"),
                         aes(year, ratio)) + 
     geom_line() + 
     facet_wrap(~comparison) +
     labs(title = "Transportation Sector"); ee.plots.trans

################################################################################
########### Policy Timeline ####################################################

library(xlsx)

policy.timeline <- read.xlsx("Energy Policy Timeline.xlsx", 
                             startRow = 3,
                             header = TRUE,
                             sheetIndex = 1)

policy.timeline <- policy.timeline %>%
     filter(!is.na(Title)) %>%
     select(Title, Subject, Topic, Date, Description) %>%
     mutate(Description = gsub("\\.", "", Description)) %>%
     rename(title = Title,
            subject = Subject,
            topic = Topic,
            year = Date,
            description = Description)

ce.plots.all + 
     geom_vline(data = policy.timeline, 
                aes(xintercept = year, color = subject))


################################################################################
###########               Comparison Data Frame                   ##############

comp.df <- rbind(rename(comp.conv.eff, ratio = ce.ratio), 
                 comp.extrn.eff)

comp.df <- comp.df %>%
     select(year, metric, comparison, sector, ratio) %>%
     arrange(year, metric, comparison, sector)

ee.plots.trans <- ggplot(filter(comp.df),
                         aes(year, ratio)) + 
     geom_line(aes(color = comparison)) + 
     facet_grid(metric~sector)

