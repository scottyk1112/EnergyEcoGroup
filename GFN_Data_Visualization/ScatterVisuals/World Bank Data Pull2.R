library(shiny)
library(data.table)
library(WDI)

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
  
######Personal Transportation Section
SchoolEnrollment_GPI <- "SE.ENR.PRSC.FM.ZS"
UrbanRoadDensity <- "IN.TRANSPORT.URBNRD.DENSIT"
RuralRoadDensity <- "IN.TRANSPORT.RURLRD.DENSIT"
RuralAccessRoads <- "IS.ROD.ALLS.ZS"
RailPassengers <- "IS.RRS.PASG.KM" #Need to normalize this by population and country size somehow
RoadPassengers <- "IS.ROD.PSGR.K6" #Need to normalize this by population and country size somehow
AirPassengers <- "IS.AIR.PSGR"     #Need to normalize this by population and country size somehow (& this is probably a lot of tourists!)
DeathsInTraffic <- "H.STA.TRAF.P5"

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

##Goods Metrics (Prelminary) 
# I'm tempted to use just 1 or two measures of material satisfaction and well-being
# One of the World economists (http://blogs.worldbank.org/impactevaluations/what-is-the-good-life-can-we-measure-it) 
# linked to http://ophi.org.uk/multidimensional-poverty-index/global-mpi-2017/mpi-data/ has asset poverty measures for many countires.
# Food_Beverages_Tobacco <- "NV.MNF.FBTO.ZS.UN"
# Textiles_Clothing <- "NV.MNF.TXTL.ZS.UN"




##Data Pull Function
WB_DataPull_Function <- function(indicator_list, CLUM_startyear, CLUM_middleyear, CLUM_endyear){
  DataFrame <- WDI(country = "all",
                   indicator = c(FoodDeficit, CerealYield, AgVAPerWorker, AgVA, FertilizerCons,
                                 FoodProdIndex, TractorPer100SqKm, AgIrrigatedLand, FishSpeciesThreatened),
                   start = CLUM_startyear, end = CLUM_endyear, extra = FALSE, cache = NULL) 
  
  DataFrame <- subset(DataFrame, year == CLUM_startyear | year == CLUM_middleyear | year == CLUM_endyear)
  
  return(DataFrame)
}


TestData <- WB_DataPull_Function(IndicatorList,2011,2011,2011)

write.csv(TestData,file="WorldBankData.csv")
