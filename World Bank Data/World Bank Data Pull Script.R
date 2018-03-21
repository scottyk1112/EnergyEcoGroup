library(shiny)
library(data.table)
library(WDI)

###############WORLD BANK DATA PULL SCRIPT#############

IndicatorAndCountries <- WDIcache()

IndicatorList <- IndicatorAndCountries[[1]]


######Food Section

Food_Metrics <- WDI(country = "all", indicator = c("NY.GNS.ICTR.GN.ZS", "EA.PRD.AGRI.KD"),
             start = 2004, end = 2011, extra = FALSE, cache = NULL)
