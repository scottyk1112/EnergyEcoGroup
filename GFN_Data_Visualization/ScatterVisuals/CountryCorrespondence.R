library(data.table)
library(dplyr)
setwd("~/GFN_Data_Visualization/")
WBData <- read.csv("WorldBankData.csv", header=TRUE, colClasses=NA)
WBData$country <- as.character(WBData$country)
WBData$country[grepl("Korea, Dem. Peopl",WBData$country)] <- "Korea, Democratic People's Republic of"

GFNtoGTAP <- read.csv("GFNtoGTAP.csv", header=TRUE, colClasses = NA)
GFNtoGTAP$AltGFN1 <- ""; GFNtoGTAP$AltGFN2 <- ""; GFNtoGTAP$AltGFN3 <- ""; GFNtoGTAP$AltGFN4 <- ""; GFNtoGTAP$AltGFN5 <- ""

#Create list with only matches to GFN list of country names
WB_filt <- WBData[WBData$country %in% GFNtoGTAP$GFN_Name,]

WB_notGFN <- as.character(WBData[3][!(WBData$country %in% GFNtoGTAP$GFN_Name),])
#drop the known and obvious country groupings in the World Bank List
WB_drop <- c("Arab World", "East Asia & Pacific (excluding high income)", 
             "Europe & Central Asia (excluding high income)", "South Asia", 
             "Central Europe and the Baltics", "European Union", "Fragile and conflict affected situations", 
             "OECD members", "Small states", "Pacific island small states", 
             "Caribbean small states", "Other small states", "Middle East & North Africa (IDA & IBRD countries)", 
             "Latin America & the Caribbean (IDA & IBRD countries)", "East Asia & Pacific (IDA & IBRD countries)", 
             "South Asia (IDA & IBRD)", "Sub-Saharan Africa (IDA & IBRD countries)", 
             "Europe & Central Asia (IDA & IBRD countries)", "Pre-demographic dividend", 
             "Early-demographic dividend", "Late-demographic dividend", "Post-demographic dividend", 
             "Euro area", "High income", "Heavily indebted poor countries (HIPC)", 
             "IBRD only", "IDA total", "IDA blend", "IDA only", "Latin America & Caribbean (excluding high income)", 
             "Least developed countries: UN classification", "Low income", 
             "Lower middle income", "Low & middle income", "Middle income", 
             "Middle East & North Africa (excluding high income)", "Upper middle income", 
             "IDA only", "Not classified", "East Asia & Pacific", "Europe & Central Asia", 
             "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa", 
             "Latin America & Caribbean", "Middle East & North Africa", "IDA & IBRD total", "North America",
             #plus countries that GFN does not have
             "Monaco", "West Bank and Gaza", "San Marino", "Kosovo")

WB_notGFN = WB_notGFN[!(WB_notGFN %in% WB_drop)]

wb <- "Bahamas, The"; gfn <- "Bahamas" 
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Congo, Dem. Rep."; gfn <- "Congo, Democratic Republic of" 
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Congo, Rep."; gfn <- "Congo"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Cote d'Ivoire"; gfn <- "Côte d'Ivoire"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Curacao"; gfn <- "Curaçao"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Egypt, Arab Rep."; gfn <- "Egypt"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Micronesia, Fed. Sts."; gfn <- "Micronesia, Federated States of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Gambia, The"; gfn <- "Gambia"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Hong Kong SAR, China"; gfn <- "China Hong Kong SAR"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Iran, Islamic Rep."; gfn <- "Iran, Islamic Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Kyrgyz Republic"; gfn <- "Kyrgyzstan"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "St. Kitts and Nevis"; gfn <- "Saint Kitts and Nevis"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Korea, Rep."; gfn <- "Korea, Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Lao PDR"; gfn <- "Lao People's Democratic Republic"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "St. Lucia"; gfn <- "Saint Lucia"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Libya"; gfn <- "Libyan Arab Jamahiriya"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "St. Martin (French part)"; gfn <- "Saint-Martin (French Part)"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Macedonia, FYR"; gfn <- "Macedonia TFYR"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Macao SAR, China"; gfn <- "China, Macao SAR"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Slovak Republic"; gfn <- "Slovakia"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Sint Maarten (Dutch part)"; gfn <- "Sint Maarten (Dutch Part)"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Tanzania"; gfn <- "Tanzania, United Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "United States"; gfn <- "United States of America"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "St. Vincent and the Grenadines"; gfn <- "Saint Vincent and Grenadines"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Venezuela, RB"; gfn <- "Venezuela, Bolivarian Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Virgin Islands (U.S.)"; gfn <- "British Virgin Islands"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Vietnam"; gfn <- "Viet Nam"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Kosovo"; gfn <- ""
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
wb <- "Yemen, Rep." ; gfn <- "Yemen"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFN = WB_notGFN[WB_notGFN!=wb]
"functions to print country names
#as a string
dput(WB_notGFN[1])
#as the whole vector
print(WB_notGFN)"


"Plan
1. Pull in GFN list of countries
2. Create a (test) list of country names/spellings alternate to GFN, NEXT to GFN list
3. Create a (test) list of country names we want to reject
3. Loop to check if WB country name is already on the list
3b. If name not on list, add to list 3, and add print of names, force error.
4. Build 2 lists of names - 1 with GFN and alt spellings, 1 of countries to filter out.
5. Figure out the aggregation between GNF to GTAP...


