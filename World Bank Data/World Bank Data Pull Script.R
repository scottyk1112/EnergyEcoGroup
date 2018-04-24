library(shiny)
library(data.table)
library(WDI)
library(dplyr)

###############WORLD BANK DATA PULL SCRIPT#############

IndicatorAndCountries <- WDIcache()

IndicatorList <- as.data.frame(IndicatorAndCountries[[1]])


######Food Section
FoodDeficit <- "SN.ITK.DFCT" #High is bad, low is good#
CerealYield <- "AG.YLD.CREL.KG" #High is good, low is bad#
AgVAPerWorker <- "EA.PRD.AGRI.KD" #High is good, low is bad#
AgVA <- "EA.PRD.LAND.KD" #High is good, low is bad#
FertilizerCons <- "AG.CON.FERT.ZS" #High is good, low is bad#
FoodProdIndex <- "AG.PRD.FOOD.XD" #High is good, low is bad#
TractorPer100SqKm <- "AG.LND.TRAC.ZS"  #High is good, low is bad#
AgIrrigatedLand <- "AG.LND.IRIG.AG.ZS" #High is good, low is bad#
FishSpeciesThreatened <- "EN.FSH.THRD.NO" #High is bad, low is good#

Food_Indicators <- c(FoodDeficit, CerealYield, AgVAPerWorker, AgVA, FertilizerCons, 
                     FoodProdIndex, TractorPer100SqKm, AgIrrigatedLand, FishSpeciesThreatened)

######Government Section
SchoolEnrollment_GPI <- "SE.ENR.PRSC.FM.ZS" #High is good, low is bad#
SanitationAccess <- "SH.STA.BASS.ZS" #High is good, low is bad#
CentralGovtDebt <- "GC.DOD.TOTL.GD.ZS" #High is bad, low is good#
FDI_NetInflows <- "BN.KLT.DINV.DRS.GDP.ZS" #High is good, low is bad#
GrossSavings <- "NY.GNS.ICTR.ZS" #High is good, low is bad#
CurrentAccountBal <- "BN.CAB.XOKA.GD.ZS" #High is bad, low is good#
EducationExpend <- "SE.XPD.TOTL.GB.ZS" #High is good, low is bad#
ExpendPerStudent <- "UIS.XUNIT.PPPCONST.1.FSGOV" #High is good, low is bad#
TeachersPrimaryEd_Trained <- "SE.PRM.TCAQ.ZS" #High is good, low is bad#
Unemployment <- "SL.UEM.TOTL.NE.ZS" #High is bad, low is good#
NetDevAssistanceReceived <- "DT.ODA.ODAT.KD" #High is bad, low is good#
Inflation <- "FP.CPI.TOTL.ZG" #High is bad, low is good#
DomesticCredit_PrivateSector <- "FS.AST.PRVT.GD.ZS" #High is good, low is bad#
HealthExpend_Total <- "SH.XPD.PUBL.ZS" #High is good, low is bad#
CPIAEconManageClusterAvg <- "IQ.CPA.ECON.XQ" #High is good, low is bad#
CPIAPublicManageClusterAvg <- "IQ.CPA.PUBS.XQ" #High is good, low is bad#
IDAResourceAllocIndex <- "IQ.CPA.IRAI.XQ" #High is good, low is bad#
PropSeatsWomenParliament <- "SG.GEN.PARL.ZS" #High is good, low is bad#
CPIASocialInclusion <- "IQ.CPA.SOCI.XQ" #High is good, low is bad#
CPIAStructuralPolicies <- "IQ.CPA.STRC.XQ" #High is good, low is bad#
  
Government_Indicators <- c(SchoolEnrollment_GPI, SanitationAccess, CentralGovtDebt, FDI_NetInflows,
                           GrossSavings, CurrentAccountBal, EducationExpend, ExpendPerStudent,
                           TeachersPrimaryEd_Trained, Unemployment, NetDevAssistanceReceived, Inflation,
                           DomesticCredit_PrivateSector, HealthExpend_Total, CPIAEconManageClusterAvg,
                           CPIAPublicManageClusterAvg, IDAResourceAllocIndex, PropSeatsWomenParliament,
                           CPIASocialInclusion, CPIAStructuralPolicies)


##Services Metrics
HospitalBeds <- "SH.MED.BEDS.ZS" #High is good, low is bad#
TelephoneSubscriptions <- "IT.MLT.MAIN.P2" #High is good, low is bad#
BroadbandSubscriptions <- "IT.NET.BBND.P2" #High is good, low is bad#
NetEnrollmentRate <- "SE.PRM.NENR" #High is good, low is bad#
TransitionRate_PrimarytoSecondary <- "SE.SEC.PROG.ZS" #High is good, low is bad#
Persistence_LastGradePrimary <- "SE.PRM.PRSL.ZS" #High is good, low is bad#
PrePrimaryEducation_Duration <- "SE.PRE.DURS" #High is good, low is bad#
Pupil_Teacher_Ratio_PrePrimary <- "SE.PRE.ENRL.TC.ZS" #High is bad, low is good#
Pupil_Teacher_Ratio_Primary <- "SE.PRM.ENRL.TC.ZS" #High is bad, low is good#
TrainedTeachers_PrePrimary <- "SE.PRE.TCAQ.ZS" #High is good, low is bad#
CPIA_SocialProtection <- "IQ.CPA.PROT.XQ" #High is good, low is bad#
SocialProtectionAdequacy <- "per_allsp.adq_pop_tot" #High is good, low is bad#
SocialInsuranceAdequacy <- "per_si_allsi.adq_pop_tot" #High is good, low is bad#
Coverage_SocialInsurance <- "per_si_allsi.cov_pop_tot" #High is good, low is bad#
Coverage_SocialInsurance_LowestQuintile <- "per_si_allsi.cov_ep_tot" #High is good, low is bad#
CPIA_FinancialSector <- "IQ.CPA.FINS.XQ" #High is good, low is bad#
  
Services_Indicators <- c(HospitalBeds, TelephoneSubscriptions, BroadbandSubscriptions, NetEnrollmentRate,
                         TransitionRate_PrimarytoSecondary, Persistence_LastGradePrimary, 
                         PrePrimaryEducation_Duration, Pupil_Teacher_Ratio_PrePrimary,
                         Pupil_Teacher_Ratio_Primary, TrainedTeachers_PrePrimary, CPIA_SocialProtection,
                         SocialProtectionAdequacy, SocialInsuranceAdequacy, Coverage_SocialInsurance, 
                         Coverage_SocialInsurance_LowestQuintile, CPIA_FinancialSector)


######Personal Transportation Section
SchoolEnrollment_GPI <- "SE.ENR.PRSC.FM.ZS" #High is good, low is bad#
UrbanRoadDensity <- "IN.TRANSPORT.URBNRD.DENSIT" #High is good, low is bad#
RuralRoadDensity <- "IN.TRANSPORT.RURLRD.DENSIT" #High is good, low is bad#
RuralAccessRoads <- "IS.ROD.ALLS.ZS" #High is good, low is bad#
RailPassengers <- "IS.RRS.PASG.KM" #High is good, low is bad# #Need to normalize this by population and country size somehow
RailPassengers_2 <- "IS.RRS.PASG.K2.PP.ZS" #High is good, low is bad# #Wasn't able to download
RoadPassengers <- "IS.ROD.PSGR.K6" #High is good, low is bad# #Need to normalize this by population and country size somehow
AirPassengers <- "IS.AIR.PSGR"    #High is good, low is bad#  #Need to normalize this by population and country size somehow (& this is probably a lot of tourists!)
DeathsInTraffic <- "H.STA.TRAF.P5" #High is bad, low is good# #Wasn't able to download

#Transport_Indicators <- c(SchoolEnrollment_GPI, UrbanRoadDensity, RuralRoadDensity, AirPassengers,
#RuralAccessRoads, RailPassengers, RailPassengers_2, RoadPassengers, DeathsInTraffic)

Transport_Indicators <- c(SchoolEnrollment_GPI, AirPassengers)


######Housing Section
HHwithonsiteh2o <- "SG.H2O.PRMS.HH.ZS" #High is good, low is bad#
H20Shortagepermo <- "IC.FRM.INFRA.IN6" #High is bad, low is good#
DistOfHausByAvailOfH2o <- "IN.POV.HH.DRKNGWATER.WITHIN" #High is good, low is bad# # Not sure what this looks like "distribution?"
HoursofPowerOutage <- "IC.ELC.OUTG.HR" #High is bad, low is good#
PersonalToilet <- "SH.STA.SMSS.ZS" #High is good, low is bad#
BldgQualityControlIndex <- "IC.DCP.BQCI" #High is good, low is bad#
CookInHaus <- "SG.COK.HOUS.ZS" #High is good, low is bad#
CookWithElec <- "SG.COK.ELEC.ZS" #High is good, low is bad#
CookWithGas <- "SG.COK.LPGN.ZS" #High is good, low is bad#

#Housing_Indicators <- c(HHwithonsiteh2o, H20Shortagepermo, DistOfHausByAvailOfH2o, HoursofPowerOutage, 
                        #PersonalToilet, BldgQualityControlIndex, CookInHaus, CookWithElec, CookWithGas)

Housing_Indicators <- c(PersonalToilet)

##Goods Metrics (Prelminary) 
# I'm tempted to use just 1 or two measures of material satisfaction and well-being
# One of the World economists (http://blogs.worldbank.org/impactevaluations/what-is-the-good-life-can-we-measure-it) 
# linked to http://ophi.org.uk/multidimensional-poverty-index/global-mpi-2017/mpi-data/ has asset poverty measures for many countires.
# Food_Beverages_Tobacco <- "NV.MNF.FBTO.ZS.UN"
# Textiles_Clothing <- "NV.MNF.TXTL.ZS.UN"


######Data Pull Function
WB_DataPull_Function <- function(indicator_list, CLUM_startyear, CLUM_middleyear, CLUM_endyear){
  DataFrame <- WDI(country = "all",
                   indicator = indicator_list,
                   start = CLUM_startyear, end = CLUM_endyear, extra = FALSE, cache = NULL) 
  
  DataFrame <- subset(DataFrame, year == CLUM_startyear | year == CLUM_middleyear | year == CLUM_endyear)
  
  return(DataFrame)
}


##Forming dataframes for each CLUM category
Food_Data <- WB_DataPull_Function(Food_Indicators, 2004, 2007, 2011)
Government_Data <- WB_DataPull_Function(Government_Indicators, 2004, 2007, 2011)
Services_Data <- WB_DataPull_Function(Services_Indicators, 2004, 2007, 2011)
Transport_Data <- WB_DataPull_Function(Transport_Indicators, 2004, 2007, 2011)
Housing_Data <- WB_DataPull_Function(Housing_Indicators, 2004, 2007, 2011)
GoodsData <- read.csv("/Users/scottkaplan1112/Box Sync/Graduate School/A_DS421/Spring 2018 Project/EnergyEcoGroup_FinalProject/World Bank Data/ass_pov_final.csv")



####Dropping WB countries not used in correspondence before forming indicators
Countries_toDrop <- read.csv("/Users/scottkaplan1112/Box Sync/Graduate School/A_DS421/Spring 2018 Project/EnergyEcoGroup_FinalProject/GFN_Data_Visualization/ScatterVisuals/DropThese.csv")
colnames(Countries_toDrop) <- c("index", "country")

Food_Data <- Food_Data[!(Food_Data$country %in% Countries_toDrop$country),]
Government_Data <- Government_Data[!(Government_Data$country %in% Countries_toDrop$country),]
Services_Data <- Services_Data[!(Services_Data$country %in% Countries_toDrop$country),]
Transport_Data <- Transport_Data[!(Transport_Data$country %in% Countries_toDrop$country),]
Housing_Data <- Housing_Data[!(Housing_Data$country %in% Countries_toDrop$country),]



####Indices Reversal (Manual)

##Food
Food_Data$SN.ITK.DFCT <- 1/Food_Data$SN.ITK.DFCT
Food_Data$EN.FSH.THRD.NO <- 1/Food_Data$EN.FSH.THRD.NO

##Government
Government_Data$GC.DOD.TOTL.GD.ZS <- 1/Government_Data$GC.DOD.TOTL.GD.ZS
Government_Data$BN.CAB.XOKA.GD.ZS <- 1/Government_Data$BN.CAB.XOKA.GD.ZS
Government_Data$SL.UEM.TOTL.NE.ZS <- 1/Government_Data$SL.UEM.TOTL.NE.ZS
Government_Data$DT.ODA.ODAT.KD <- 1/Government_Data$DT.ODA.ODAT.KD
Government_Data$FP.CPI.TOTL.ZG <- 1/Government_Data$FP.CPI.TOTL.ZG

##Services
Services_Data$SE.PRE.ENRL.TC.ZS <- 1/Services_Data$SE.PRE.ENRL.TC.ZS
Services_Data$SE.PRM.ENRL.TC.ZS <- 1/Services_Data$SE.PRM.ENRL.TC.ZS

##Transport (none)

##Housing (none)

##Goods
#GoodsData$ass_pov_extr <- 1/(GoodsData$ass_pov_extr+1)

##NA Removal Function
NARemove_Fun <- function(data, NA_factor){
  
  ##Count NAs by column to remove columns with lots of NAs
  na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
  na_count_df <- data.frame(na_count)
  
  ##Remove columns where more than half of observations are NAs
  na_count_df <- subset(na_count_df, na_count < nrow(data)/NA_factor)
  rownames_tokeep <- rownames(na_count_df)
  
  ##Keep columns without big number of NAs
  Data_NoNAs <- data[rownames_tokeep]
  
  return(Data_NoNAs)
  
}

FoodData_NoNAs <- NARemove_Fun(Food_Data, 2)
GovernmentData_NoNAs <- NARemove_Fun(Government_Data, 2)
ServicesData_NoNAs <- NARemove_Fun(Services_Data, 2)
TransportData_NoNAs <- NARemove_Fun(Transport_Data, 2)
HousingData_NoNAs <- NARemove_Fun(Housing_Data, 1.25)






####Max/Min function calculation####
MaxMin_Fun <- function(data, category){
  
colnames_important <- data[,-c(1:3)]
datamatrix <- matrix(ncol = ncol(colnames_important), nrow = nrow(data))
for(i in 1:ncol(colnames_important)){
  datamatrix[,i] <- data[,i+3]/max(data[,i+3], na.rm = TRUE)
}

datamatrix <- as.data.frame(datamatrix)
colnames(datamatrix) <- colnames(colnames_important)
datamatrix$MaxMin_Index <- rowMeans(datamatrix, na.rm = TRUE)
colnames(datamatrix)[ncol(datamatrix)] <- "MaxMin_Index"
datamatrix$CLUM_category <- category

datamatrix <- cbind(data[,c(1:3)], datamatrix)
datamatrix$NAPercent <- (rowSums(is.na(datamatrix))/max(rowSums(is.na(datamatrix))))*100

datamatrix <- datamatrix[,c(2:3,(ncol(datamatrix)-2):ncol(datamatrix))]
return(datamatrix)

}


FoodData_MaxMin <- MaxMin_Fun(FoodData_NoNAs, "Food")
GovernmentData_MaxMin <- MaxMin_Fun(GovernmentData_NoNAs, "Government")
ServicesData_MaxMin <- MaxMin_Fun(ServicesData_NoNAs, "Services")
TransportData_MaxMin <- MaxMin_Fun(TransportData_NoNAs, "Transport")

##For Housing data, only one column and already 0 to 1
colnames(HousingData_NoNAs) <- c("iso2c", "country", "MaxMin_Index", "year")
HousingData_NoNAs$MaxMin_Index <- HousingData_NoNAs$MaxMin_Index/100
HousingData_NoNAs$CLUM_category <- "Housing"
HousingData_NoNAs$NAPercent <- (rowSums(is.na(HousingData_NoNAs))/max(rowSums(is.na(HousingData_NoNAs))))*100
HousingData_MaxMin <- HousingData_NoNAs[,c(2:6)]

##For Goods Data, just rename column
colnames(GoodsData) <- c("country", "year", "MaxMin_Index")
GoodsData$MaxMin_Index <- GoodsData$MaxMin_Index/100
GoodsData$CLUM_category <- "Goods"
GoodsData$NAPercent <- (rowSums(is.na(GoodsData))/max(rowSums(is.na(GoodsData))))*100
GoodsData_MaxMin <- GoodsData[,c(1:3,6:7)]

##Binding Data together for single spreadsheet
MaxMinData <- rbind(FoodData_MaxMin, GovernmentData_MaxMin, ServicesData_MaxMin, 
                    TransportData_MaxMin, HousingData_MaxMin, GoodsData_MaxMin)




########Now for z-score stuff
####Max/Min function calculation####
ZScore_Fun <- function(data, category){
  
  colnames_important <- data[,-c(1:3)]
  datamatrix <- matrix(ncol = ncol(colnames_important), nrow = nrow(data))
  for(i in 1:ncol(colnames_important)){
    datamatrix[,i] <- scale(data[,i+3])
  }
  
  datamatrix <- as.data.frame(datamatrix)
  colnames(datamatrix) <- colnames(colnames_important)
  datamatrix$MaxMin_Index <- rowMeans(datamatrix, na.rm = TRUE)
  colnames(datamatrix)[ncol(datamatrix)] <- "ZScore_Index"
  datamatrix$CLUM_category <- category
  
  datamatrix <- cbind(data[,c(1:3)], datamatrix)
  
  datamatrix <- datamatrix[,c(2:3,(ncol(datamatrix)-1):ncol(datamatrix))]
  return(datamatrix)
  
}


FoodData_ZScore <- ZScore_Fun(FoodData_NoNAs, "Food")
GovernmentData_ZScore <- ZScore_Fun(GovernmentData_NoNAs, "Government")
ServicesData_ZScore <- ZScore_Fun(ServicesData_NoNAs, "Services")
TransportData_ZScore <- ZScore_Fun(TransportData_NoNAs, "Transport")


##For Housing data, only one column and already 0 to 1
ZScore_Index <- scale(HousingData_NoNAs$MaxMin_Index)
HousingData_ZScore <- cbind(HousingData_NoNAs[,c(2, 4:5)], ZScore_Index)

##For Goods Data, just rename column
ZScore_Index <- scale(GoodsData$MaxMin_Index)
GoodsData_ZScore <- cbind(GoodsData[,c(1:2, 6)], ZScore_Index)

##Binding Data together for single spreadsheet
ZScoreData <- rbind(FoodData_ZScore, GovernmentData_ZScore, ServicesData_ZScore, 
                    TransportData_ZScore, HousingData_ZScore, GoodsData_ZScore)



##Combining MaxMin and Z-score datasets
IndicesData <- left_join(ZScoreData, MaxMinData, by = c("country", "year", "CLUM_category"))

write.csv(IndicesData, "/Users/scottkaplan1112/Box Sync/Graduate School/A_DS421/Spring 2018 Project/EnergyEcoGroup_FinalProject/World Bank Data/IndicesData.csv")
