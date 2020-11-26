##### Economic Cost Calculation Function #####

EconCostCalc=function(PUB_Data, Tax_Data, t_Ld, d_CV, CV_SME, CV_LE, OEE_FL, p_pessimistic, p_neutral,
                      p_optimistic){
  # PUB_Data Expenses----

  # Lockdown Act
  KaExpLd=sum(PUB_Data$`Fragliche Lohnsumme`*PUB_Data$Exposure_Ld*0.8*t_Ld)
  
  # No Lockdown Act (3 scenarios)
  KaExpNoLd1=sum(PUB_Data$`Fragliche Lohnsumme`*PUB_Data$Exposure_NoLd_S1*0.8*t_Ld)
  KaExpNoLd2=sum(PUB_Data$`Fragliche Lohnsumme`*PUB_Data$Exposure_NoLd_S2*0.8*t_Ld)
  KaExpNoLd3=sum(PUB_Data$`Fragliche Lohnsumme`*PUB_Data$Exposure_NoLd_S3*0.8*t_Ld)
  
  # Credit Defaults----
  
  # Lockdown Act
  GovernmentCoverage_CV_LE=0.85
  CredDefLd=d_CV*CV_SME+d_CV*GovernmentCoverage_CV_LE*
    CV_LE
  
  # No Lockdown Act (3 scenarios)
  CredDefNoLd1=0
  CredDefNoLd2=0
  CredDefNoLd3=0
  
  # Corporate Profit Tax Shortfalls----
  
  # Lockdown Act
  CorpProfTaxShortLd=sum(Tax_Data$`Erwartete Gewinnsteuereinnahmen ohne Corona`*
                           Tax_Data$Exposure_Ld*t_Ld/12, na.rm = T)
  
  # No Lockdown Act (3 scenarios)
  CorpProfTaxShortNoLd1=sum(Tax_Data$`Erwartete Gewinnsteuereinnahmen ohne Corona`*
                              Tax_Data$Exposure_NoLd_S1*t_Ld/12, na.rm = T)
  CorpProfTaxShortNoLd2=sum(Tax_Data$`Erwartete Gewinnsteuereinnahmen ohne Corona`*
                              Tax_Data$Exposure_NoLd_S2*t_Ld/12, na.rm = T)
  CorpProfTaxShortNoLd3=sum(Tax_Data$`Erwartete Gewinnsteuereinnahmen ohne Corona`*
                              Tax_Data$Exposure_NoLd_S3*t_Ld/12, na.rm = T)
  
  # Value Added Tax Shortfalls----
  
  # Lockdown Act
  ValAddTaxShortLd=sum(Tax_Data$`Erwartete Mehrwertsteuereinnahmen ohne Corona`*
                         Tax_Data$Exposure_Ld*t_Ld/12, na.rm = T)
  
  # No Lockdown Act (3 scenarios)
  ValAddTaxShortNoLd1=sum(Tax_Data$`Erwartete Mehrwertsteuereinnahmen ohne Corona`*
                            Tax_Data$Exposure_NoLd_S1*t_Ld/12, na.rm = T)
  ValAddTaxShortNoLd2=sum(Tax_Data$`Erwartete Mehrwertsteuereinnahmen ohne Corona`*
                            Tax_Data$Exposure_NoLd_S2*t_Ld/12, na.rm = T)
  ValAddTaxShortNoLd3=sum(Tax_Data$`Erwartete Mehrwertsteuereinnahmen ohne Corona`*
                            Tax_Data$Exposure_NoLd_S3*t_Ld/12, na.rm = T)
  
  # Other Extraordinary Expenses----
  
  # Lockdown Act
  OEELd=OEE_FL
  
  # No Lockdown Act (3 scenarios)
  OEENoLd1=0
  OEENoLd2=0
  OEENoLd3=0
  
  #### Economic Cost Table ####
  
  # Sum the costs up
  EconCostsLd=KaExpLd+CredDefLd+CorpProfTaxShortLd+ValAddTaxShortLd+OEELd
  EconCostsNoLd1=KaExpNoLd1+CredDefNoLd1+CorpProfTaxShortNoLd1+ValAddTaxShortNoLd1+OEENoLd1
  EconCostsNoLd2=KaExpNoLd2+CredDefNoLd2+CorpProfTaxShortNoLd2+ValAddTaxShortNoLd2+OEENoLd2
  EconCostsNoLd3=KaExpNoLd3+CredDefNoLd3+CorpProfTaxShortNoLd3+ValAddTaxShortNoLd3+OEENoLd3
  
  # Create a error alert in case the priors do not add up to 1
  if(p_pessimistic+p_neutral+p_optimistic!=1) stop('The priors do not add up to 1')
  Prior=c(p_pessimistic, p_neutral, p_optimistic)
  
  # Create the Economic Table
  EconCostsLd=c(EconCostsLd,EconCostsLd,EconCostsLd)
  EconCostsNoLd=c(EconCostsNoLd1,EconCostsNoLd2,EconCostsNoLd3)
  AddEconCostsLd=EconCostsLd-EconCostsNoLd
  AddEconCostsLdPerWeek=AddEconCostsLd/(33/7)
  Scenario=c("pessimistic", "neutral","optimistic")
  EconShockExp=c("low", "medium", "high")
  EconCost=data.frame(Prior,EconCostsLd,EconCostsNoLd, AddEconCostsLd, AddEconCostsLdPerWeek,
                      Scenario, EconShockExp)
  EconCostMrd=cbind(Prior,EconCost[,c(-1,-6,-7)]/1000000000,Scenario, EconShockExp)
  return(EconCost)
  
}

save(EconCostCalc, file="Functions/f_EconCostCalc.R")
