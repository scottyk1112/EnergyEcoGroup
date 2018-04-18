library(shiny)
library(data.table)
library(WDI)
library(dplyr)

###############WORLD BANK DATA PULL SCRIPT#############

IndicatorAndCountries <- WDIcache()

IndicatorList <- as.data.frame(IndicatorAndCountries[[1]])


######Food Section
FoodDeficit <- "SN.ITK.DFCT"
CerealYield <- "AG.YLD.CREL.KG"
AgVAPerWorker <- "EA.PRD.AGRI.KD"
AgVA <- "EA.PRD.LAND.KD"
FertilizerCons <- "AG.CON.FERT.ZS"
FoodProdIndex <- "AG.PRD.FOOD.XD"
TractorPer100SqKm <- "AG.LND.TRAC.ZS" 
AgIrrigatedLand <- "AG.LND.IRIG.AG.ZS"
FishSpeciesThreatened <- "EN.FSH.THRD.NO"

Food_Indicators <- c(FoodDeficit, CerealYield, AgVAPerWorker, AgVA, FertilizerCons, 
                     FoodProdIndex, TractorPer100SqKm, AgIrrigatedLand, FishSpeciesThreatened)

######Government Section
SchoolEnrollment_GPI <- "SE.ENR.PRSC.FM.ZS"
SanitationAccess <- "SH.STA.BASS.ZS"
CentralGovtDebt <- "GC.DOD.TOTL.GD.ZS"
FDI_NetInflows <- "BN.KLT.DINV.DRS.GDP.ZS"
GrossSavings <- "NY.GNS.ICTR.ZS"
CurrentAccountBal <- "BN.CAB.XOKA.GD.ZS"
EducationExpend <- "SE.XPD.TOTL.GB.ZS"
ExpendPerStudent <- "UIS.XUNIT.PPPCONST.1.FSGOV"
TeachersPrimaryEd_Trained <- "SE.PRM.TCAQ.ZS"
Unemployment <- "SL.UEM.TOTL.NE.ZS"
NetDevAssistanceReceived <- "DT.ODA.ODAT.KD"
Inflation <- "FP.CPI.TOTL.ZG"
DomesticCredit_PrivateSector <- "FS.AST.PRVT.GD.ZS"
HealthExpend_Total <- "SH.XPD.PUBL.ZS"
CPIAEconManageClusterAvg <- "IQ.CPA.ECON.XQ"
CPIAPublicManageClusterAvg <- "IQ.CPA.PUBS.XQ"
IDAResourceAllocIndex <- "IQ.CPA.IRAI.XQ"
PropSeatsWomenParliament <- "SG.GEN.PARL.ZS"
CPIASocialInclusion <- "IQ.CPA.SOCI.XQ"
CPIAStructuralPolicies <- "IQ.CPA.STRC.XQ"
  
Government_Indicators <- c(SchoolEnrollment_GPI, SanitationAccess, CentralGovtDebt, FDI_NetInflows,
                           GrossSavings, CurrentAccountBal, EducationExpend, ExpendPerStudent,
                           TeachersPrimaryEd_Trained, Unemployment, NetDevAssistanceReceived, Inflation,
                           DomesticCredit_PrivateSector, HealthExpend_Total, CPIAEconManageClusterAvg,
                           CPIAPublicManageClusterAvg, IDAResourceAllocIndex, PropSeatsWomenParliament,
                           CPIASocialInclusion, CPIAStructuralPolicies)


##Services Metrics
HospitalBeds <- "SH.MED.BEDS.ZS"
TelephoneSubscriptions <- "IT.MLT.MAIN.P2"
BroadbandSubscriptions <- "IT.NET.BBND.P2"
NetEnrollmentRate <- "SE.PRM.NENR"
TransitionRate_PrimarytoSecondary <- "SE.SEC.PROG.ZS"
Persistence_LastGradePrimary <- "SE.PRM.PRSL.ZS"
PrePrimaryEducation_Duration <- "SE.PRE.DURS"
Pupil_Teacher_Ratio_PrePrimary <- "SE.PRE.ENRL.TC.ZS"
Pupil_Teacher_Ratio_Primary <- "SE.PRM.ENRL.TC.ZS"
TrainedTeachers_PrePrimary <- "SE.PRE.TCAQ.ZS"
CPIA_SocialProtection <- "IQ.CPA.PROT.XQ"
SocialProtectionAdequacy <- "per_allsp.adq_pop_tot"
SocialInsuranceAdequacy <- "per_si_allsi.adq_pop_tot"
Coverage_SocialInsurance <- "per_si_allsi.cov_pop_tot"
Coverage_SocialInsurance_LowestQuintile <- "per_si_allsi.cov_ep_tot"
CPIA_FinancialSector <- "IQ.CPA.FINS.XQ"
  
Services_Indicators <- c(HospitalBeds, TelephoneSubscriptions, BroadbandSubscriptions, NetEnrollmentRate,
                         TransitionRate_PrimarytoSecondary, Persistence_LastGradePrimary, 
                         PrePrimaryEducation_Duration, Pupil_Teacher_Ratio_PrePrimary,
                         Pupil_Teacher_Ratio_Primary, TrainedTeachers_PrePrimary, CPIA_SocialProtection,
                         SocialProtectionAdequacy, SocialInsuranceAdequacy, Coverage_SocialInsurance, 
                         Coverage_SocialInsurance_LowestQuintile, CPIA_FinancialSector)


######Personal Transportation Section
SchoolEnrollment_GPI <- "SE.ENR.PRSC.FM.ZS"
#UrbanRoadDensity <- "IN.TRANSPORT.URBNRD.DENSIT"
#RuralRoadDensity <- "IN.TRANSPORT.RURLRD.DENSIT"
#RuralAccessRoads <- "IS.ROD.ALLS.ZS"
RailPassengers <- "IS.RRS.PASG.KM" #Need to normalize this by population and country size somehow
#RailPassengers_2 <- "IS.RRS.PASG.K2.PP.ZS" #Wasn't able to download
RoadPassengers <- "IS.ROD.PSGR.K6" #Need to normalize this by population and country size somehow
AirPassengers <- "IS.AIR.PSGR"     #Need to normalize this by population and country size somehow (& this is probably a lot of tourists!)
#DeathsInTraffic <- "H.STA.TRAF.P5" #Wasn't able to download

Transport_Indicators <- c(SchoolEnrollment_GPI, RailPassengers, RoadPassengers, AirPassengers)


######Housing Section
HHwithonsiteh2o <- "SG.H2O.PRMS.HH.ZS"
H20Shortagepermo <- "IC.FRM.INFRA.IN6"
DistOfHausByAvailOfH2o <- "IN.POV.HH.DRKNGWATER.WITHIN" # Not sure what this looks like "distribution?"
HoursofPowerOutage <- "IC.ELC.OUTG.HR"
PersonalToilet <- "SH.STA.SMSS.ZS"
BldgQualityControlIndex <- "IC.DCP.BQCI"
CookInHaus <- "SG.COK.HOUS.ZS"
CookWithElec <- "SG.COK.ELEC.ZS"
CookWithGas <- "SG.COK.LPGN.ZS"

Housing_Indicators <- c(HHwithonsiteh2o, H20Shortagepermo, DistOfHausByAvailOfH2o, HoursofPowerOutage, 
                        PersonalToilet, BldgQualityControlIndex, CookInHaus, CookWithElec, CookWithGas)

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
TransportData_NoNAs <- NARemove_Fun(Transport_Data, 1.5)
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

datamatrix <- datamatrix[,c(2:3,(ncol(datamatrix)-1):ncol(datamatrix))]
return(datamatrix)

}


FoodData_MaxMin <- MaxMin_Fun(FoodData_NoNAs, "Food")
GovernmentData_MaxMin <- MaxMin_Fun(GovernmentData_NoNAs, "Government")
ServicesData_MaxMin <- MaxMin_Fun(ServicesData_NoNAs, "Services")
TransportData_MaxMin <- MaxMin_Fun(TransportData_NoNAs, "Transport")

##For Housing data, only one column and already 0 to 1
colnames(HousingData_NoNAs) <- c("iso2c", "country", "year", "MaxMin_Index")
HousingData_NoNAs$MaxMin_Index <- HousingData_NoNAs$MaxMin_Index/100
HousingData_NoNAs$CLUM_category <- "Housing"
HousingData_MaxMin <- HousingData_NoNAs[,c(2:5)]

##For Goods Data, just rename column
colnames(GoodsData) <- c("country", "year", "MaxMin_Index")
GoodsData$MaxMin_Index <- GoodsData$MaxMin_Index/100
GoodsData$CLUM_category <- "Goods"
GoodsData_MaxMin <- GoodsData

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
HousingData_ZScore <- cbind(HousingData_NoNAs[,c(2:3, 5)], ZScore_Index)

##For Goods Data, just rename column
ZScore_Index <- scale(GoodsData$MaxMin_Index)
GoodsData_ZScore <- cbind(GoodsData[,c(1:2, 4)], ZScore_Index)

##Binding Data together for single spreadsheet
ZScoreData <- rbind(FoodData_ZScore, GovernmentData_ZScore, ServicesData_ZScore, 
                    TransportData_ZScore, HousingData_ZScore, GoodsData_ZScore)



##Combining MaxMin and Z-score datasets
IndicesData <- left_join(ZScoreData, MaxMinData, by = c("country", "year", "CLUM_category"))

write.csv(IndicesData, "/Users/scottkaplan1112/Box Sync/Graduate School/A_DS421/Spring 2018 Project/EnergyEcoGroup_FinalProject/World Bank Data/IndicesData.csv")
