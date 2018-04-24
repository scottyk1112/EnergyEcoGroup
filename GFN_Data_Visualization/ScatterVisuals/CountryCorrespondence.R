library(data.table)
library(dplyr)
#setwd("./GFN_Data_Visualization/ScatterVisuals")

#read in World Bank Data
WBData <- read.csv("IndicesData.csv", header=TRUE, colClasses=NA)
WBData$country <- as.character(WBData$country)
#Ass_pov <- read.csv("ass_pov_final.csv", header=TRUE, colClasses=NA)

#deal with weird symbol in country name
WBData$country[grepl("Korea, Dem. Peopl",WBData$country)] <- "Korea, Democratic People's Republic of"

#Get correspondence table and add fields for alt spellings
GFNtoGTAP <- read.csv("GFNtoGTAP.csv", header=TRUE, colClasses = NA)
GFNtoGTAP$AltGFN1 <- ""; GFNtoGTAP$AltGFN2 <- ""; GFNtoGTAP$AltGFN3 <- ""; GFNtoGTAP$AltGFN4 <- ""; GFNtoGTAP$AltGFN5 <- ""
GFN_Pop <- read.csv("GFN_Population.csv", header=TRUE, colClasses = NA)

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
             "Rest of Oceania", "Rest of East Asia", "Rest of Southeast Asia", 
             "Rest of South Asia", "Rest of North America", "Rest of South America", "Rest of Central America", 
             "Caribbean", "Rest of EFTA", "Rest of Eastern Europe", "Rest of Europe", "Rest of Former Soviet Union", 
             "Rest of Western Asia", "Rest of North Africa", "Rest of Western Africa", "Central Africa", 
             "South Central Africa", "Rest of Eastern Africa", "Rest of South African Customs Union", 
             "Rest of the World",
             #plus countries that GFN does not have
             "Monaco", "West Bank and Gaza", "San Marino", "Kosovo",
             #Plus country GFN has but we don't want
             "World")

#write.csv(WB_drop, "DropThese.csv")

#filter to end up with remainders not in GFN or drop
WB_notGFNlist <- WBData$country[!(WBData$country %in% GFNtoGTAP$GFN_Name)]
WB_notGFNlist <- WB_notGFNlist[!(WB_notGFNlist %in% WB_drop)]
#already in GTAP gropuings (from Nat)
#####WB_notGFNlist <- WB_notGFNlist[!(WB_notGFNlist %in% GFNtoGTAP$GTAP_name)]

#Update spellings to GFN, and then drop from the 'remainder' list
wb <- "Bahamas, The"; gfn <- "Bahamas" 
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Congo, Dem. Rep."; gfn <- "Congo, Democratic Republic of" 
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Congo, Rep."; gfn <- "Congo"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Cote d'Ivoire"; gfn <- "Côte d'Ivoire"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Cote dIvoire"; gfn <- "Côte d'Ivoire"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Curacao"; gfn <- "Curaçao"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Egypt, Arab Rep."; gfn <- "Egypt"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Micronesia, Fed. Sts."; gfn <- "Micronesia, Federated States of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Gambia, The"; gfn <- "Gambia"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Hong Kong SAR, China"; gfn <- "China Hong Kong SAR"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Hong Kong, Special Administrative Region of China"; gfn <- "China Hong Kong SAR"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Iran, Islamic Rep."; gfn <- "Iran, Islamic Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Iran"; gfn <- "Iran, Islamic Republic of"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Kyrgyz Republic"; gfn <- "Kyrgyzstan"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "St. Kitts and Nevis"; gfn <- "Saint Kitts and Nevis"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Korea, Rep."; gfn <- "Korea, Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "South Korea"; gfn <- "Korea, Republic of"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Lao PDR"; gfn <- "Lao People's Democratic Republic"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Laos"; gfn <- "Lao People's Democratic Republic"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "St. Lucia"; gfn <- "Saint Lucia"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Libya"; gfn <- "Libyan Arab Jamahiriya"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "St. Martin (French part)"; gfn <- "Saint-Martin (French Part)"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Macedonia, FYR"; gfn <- "Macedonia TFYR"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Macao SAR, China"; gfn <- "China, Macao SAR"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Slovak Republic"; gfn <- "Slovakia"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Sint Maarten (Dutch part)"; gfn <- "Sint Maarten (Dutch Part)"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Tanzania"; gfn <- "Tanzania, United Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Taiwan"; gfn <- "Taiwan, Republic of China"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Taiwan, Republic of China"; gfn <- "Taiwan, Republic of China"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "United States"; gfn <- "United States of America"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "St. Vincent and the Grenadines"; gfn <- "Saint Vincent and Grenadines"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Venezuela, RB"; gfn <- "Venezuela, Bolivarian Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Venezuela (Bolivarian Republic of)"; gfn <- "Venezuela, Bolivarian Republic of"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Virgin Islands (U.S.)"; gfn <- "US Virgin Islands"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Vietnam"; gfn <- "Viet Nam"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Yemen, Rep." ; gfn <- "Yemen"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]

#Throw exception and list if any countries haven't been dealt with
ifelse(length(WB_notGFNlist) == 0, print("All present and accounted for"),
       stop(print(c('Eli says:: Error:: These countries are not dealt with:',WB_notGFNlist))))

"Reminder for Eli: functions used to print country names to build list #as a string dput(WB_notGFN[1]) #as the whole vector print(WB_notGFN)"

#loop through and update country names to GFN spellings 
for (i in 1:length(WBData[,1])) {
  for (j in 1:length(GFNtoGTAP[,1])) {ifelse(WBData$country[i] == GFNtoGTAP$AltGFN1[j],
                                             WBData$country[i] <- as.character(GFNtoGTAP$GFN_Name[j]),
                                             WBData$country[i] <- WBData$country[i])
  }
}

for (i in 1:length(WBData[,1])) {
  for (j in 1:length(GFNtoGTAP[,1])) {ifelse(WBData$country[i] == GFNtoGTAP$AltGFN2[j],
                                             WBData$country[i] <- as.character(GFNtoGTAP$GFN_Name[j]),
                                             WBData$country[i] <- WBData$country[i])
  }
}

# Create subset with only GFN and get rid of 'World'
WB_filt <-WBData[WBData$country %in% GFNtoGTAP$GFN_Name,]
WB_filt <- WB_filt[!WB_filt$country=="World",]

#Table of WB straight to GTAP
WBGFN_GTAP <- WB_filt[WB_filt$country %in% GFNtoGTAP$GTAP_name,]
#Table of WB not 1:1 in GTAP via GFN name
WBGFN_notGTAP <- WB_filt[!(WB_filt$country %in% GFNtoGTAP$GTAP_name),]

#Subset population table for just years included
years <- c(2004,2007,2011)
GFN_yr_Pop <- GFN_Pop[GFN_Pop$Year %in% years,]

#Initialize Population column and fill population column in WBGFN_notGTAP
WBGFN_notGTAP$Population <- NA
for (i in 1:length(WBGFN_notGTAP[, 1])) {
  for (j in 1:length(GFN_yr_Pop[, 1])) {
    ifelse(WBGFN_notGTAP$country[i] == GFN_Pop$GFN_Country[j],
      WBGFN_notGTAP$Population[i] <- GFN_Pop$Population[j],
      "nope")
  }
}

# Check countries with no population/weighting data
ZeroPopTest <- WBGFN_notGTAP$country[is.na(WBGFN_notGTAP$Population)]
print(ZeroPopTest)
#Have to filter out countries with no population (or other weighting data)
WBGFN_notGTAP <- WBGFN_notGTAP[!is.na(WBGFN_notGTAP$Population),]


#Initialize GTAP_Region column and add GTAP Regions to WBGFN and for Aggregating WBGFN_noGTAP
WBGFN_notGTAP$GTAP_Region <- "R"
for (i in 1:length(WBGFN_notGTAP[, 1])) {
  for (j in 1:length(GFNtoGTAP[,1])) {
    ifelse(WBGFN_notGTAP$country[i] == GFNtoGTAP$GFN_Name[j],
           WBGFN_notGTAP$GTAP_Region[i] <- as.character(GFNtoGTAP$GTAP_name[j]),
           "dunno")
  }
}

#Create table of aggregated indicat ors by GTAP Region, na's omitted
WBGTAP_weighted <- as.data.frame(t(sapply(split(WBGFN_notGTAP, list(WBGFN_notGTAP$GTAP_Region, WBGFN_notGTAP$CLUM_category, WBGFN_notGTAP$year)),
                                          function(x) apply(x[,c(5,7:8)], 2, weighted.mean, x$Population, na.rm = TRUE))))

setDT(WBGTAP_weighted, keep.rownames = TRUE )[]
colnames(WBGTAP_weighted)[1] <- "REgion_year_CLUM"
WBGTAP_weighted <- cSplit(WBGTAP_weighted, "REgion_year_CLUM", ".")
colnames(WBGTAP_weighted)[4] <- "GTAP_Region"
colnames(WBGTAP_weighted)[5] <- "CLUM_category"
colnames(WBGTAP_weighted)[6] <- "year"
#GTAP_WBweighted <- cbind(WBGTAP_weighted[,1], year = WBGTAP_weighted$year, WBGTAP_weighted[,2:length(WBGTAP_weighted)])
#GTAP_WBweighted$GTAP_Region <- "ph"; GTAP_WBweighted$year <- 1111; GTAP_WBweighted$CLUM_category <- 'nums' 
#cbind(c(WBGTAP_weighted$GTAP_Region <- "ph", WBGTAP_weighted$CLUM_category <- 'nums', WBGTAP_weighted$year <- 1111, WBGTAP_weighted))

#x <- strsplit(WBGTAP_weighted$REgion_year_CLUM, ".", fixed = TRUE) 

#Set up GFN-GTAP table for merge
WBGFN_GTAP <- subset(WBGFN_GTAP, select= c(-X.1,-X.2,-country_orCode,-GTAP_Region))
colnames(WBGFN_GTAP)[1] <- "GTAP_Region"

#Stick 'em together
GTAP_WBweighted <- rbind(WBGTAP_weighted,WBGFN_GTAP)

write.csv(GTAP_WBweighted, "WBIndicators_byGTAP.csv")
