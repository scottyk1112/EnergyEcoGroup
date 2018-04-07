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
  

##Goods Metrics (Prelminary) 
Food_Beverages_Tobacco <- "NV.MNF.FBTO.ZS.UN"
Textiles_Clothing <- "NV.MNF.TXTL.ZS.UN"


##Data Pull Function
WB_DataPull_Function <- function(indicator_list, CLUM_startyear, CLUM_middleyear, CLUM_endyear){
  DataFrame <- WDI(country = "all",
                   indicator = c(FoodDeficit, CerealYield, AgVAPerWorker, AgVA, FertilizerCons,
                                 FoodProdIndex, TractorPer100SqKm, AgIrrigatedLand, FishSpeciesThreatened),
                   start = CLUM_startyear, end = CLUM_endyear, extra = FALSE, cache = NULL) 
  
  DataFrame <- subset(DataFrame, year == CLUM_startyear | year == CLUM_middleyear | year == CLUM_endyear)
  
  return(DataFrame)
}


