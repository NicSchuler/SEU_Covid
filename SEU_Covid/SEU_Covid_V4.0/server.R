# Packages--------------
library(plyr)
library(ggplot2)
library(tidyverse)
library(shiny)
library(reshape2)
library(rsconnect)

setwd("./")

# prepare own functions--------------
# utilities--------
# linear utility function
# define a function to calculate the linear utility of a scenario
linearUtility = function(EconCost, HealthCost){
  utility = round(-(EconCost + HealthCost)/1000000000,3)
  return(utility)
}

# quadratic utility function
# define a function to calculate the quadratic utility of a scenario
quadraticUtility = function(EconCost, HealthCost){
  utility = round(-((EconCost + HealthCost)/1000000000)^2,3)
  return(utility)
}

# Gather Cost and Health data--------
gatherCostHealth = function(EconCostScenario, EconCostPrior, EconCostLD, EconCostNM, HealthScenarioR, HealthScenarioCFR, HealthPrior, HealthCostsLD, HealthCostsNM){
  # create a container for the final table
  TotalCost = na.omit(data.frame(EconImpact = NA, Fatality = NA, Reproduction = NA, Prior = NA, EconCostLD = NA, EconCostNM = NA, HealthCostsLD = NA, HealthCostsNM = NA))
  
  # Outer Loop: Economic Scenarios
  for (i in 1:length(EconCostScenario)){
    
    # Inner Loop: Health Scenarios
    for (j in 1:length(HealthScenarioR)){
      newline = data.frame(EconImpact = EconCostScenario[i], Fatality = HealthScenarioCFR[j], Reproduction = HealthScenarioR[j],  
                           Prior = EconCostPrior[i]*HealthPrior[j], EconCostLD = EconCostLD[i], EconCostNM = EconCostNM[i],
                           HealthCostsLD = HealthCostsLD[j], HealthCostsNM = HealthCostsNM[j])
      TotalCost = rbind(TotalCost,newline)
    }
  }
  return(TotalCost)
}

# Econ Cost Calculation function--------
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

# Health Cost Calculation function---------
get_health_costs <- function(
  INCUBATION_TIME_DAYS = 10,
  REPRODUCTION_RATE_DISCOUNT_LOCKDOWN = .45,
  COST_LOST_YEAR = 100000,
  BASIC_REPRODUCTION_NUMBER_LOW = 1.4,
  BASIC_REPRODUCTION_NUMBER_MEDIUM = 2.79,
  BASIC_REPRODUCTION_NUMBER_HIGH = 3.3,
  PERIOD_OF_INTEREST_DAYS = 33
) {
  ##########################################
  ## import data
  ##########################################
  load("lifeExpectancy.Rdata")
  load("caseFatalityGrouped.RData")
  load("cases.RData")
  
  
  ##########################################
  ## calculate cases
  ##########################################
  cases_partial_lockdown_low_rr <- cases
  cases_partial_lockdown_medium_rr <- cases
  cases_partial_lockdown_high_rr <- cases
  
  cases_full_lockdown_low_rr <- cases
  cases_full_lockdown_medium_rr <- cases
  cases_full_lockdown_high_rr <- cases
  
  for (i in c(1:PERIOD_OF_INTEREST_DAYS)) {
    for (ii in c(1:9)) {
      cases_partial_lockdown_low_rr[ii,ncol(cases)+i] <- round(cases_partial_lockdown_low_rr[ii,(ncol(cases)+i)-INCUBATION_TIME_DAYS] * BASIC_REPRODUCTION_NUMBER_LOW, digits = 0)
      cases_partial_lockdown_medium_rr[ii,ncol(cases)+i] <- round(cases_partial_lockdown_medium_rr[ii,(ncol(cases)+i)-INCUBATION_TIME_DAYS] * BASIC_REPRODUCTION_NUMBER_MEDIUM, digits = 0)
      cases_partial_lockdown_high_rr[ii,ncol(cases)+i] <- round(cases_partial_lockdown_high_rr[ii,(ncol(cases)+i)-INCUBATION_TIME_DAYS] * BASIC_REPRODUCTION_NUMBER_HIGH, digits = 0)
      
      cases_full_lockdown_low_rr[ii,ncol(cases)+i] <- round(cases_full_lockdown_low_rr[ii,(ncol(cases)+i)-INCUBATION_TIME_DAYS] * BASIC_REPRODUCTION_NUMBER_LOW * REPRODUCTION_RATE_DISCOUNT_LOCKDOWN, digits = 0)
      cases_full_lockdown_medium_rr[ii,ncol(cases)+i] <- round(cases_full_lockdown_medium_rr[ii,(ncol(cases)+i)-INCUBATION_TIME_DAYS] * BASIC_REPRODUCTION_NUMBER_MEDIUM * REPRODUCTION_RATE_DISCOUNT_LOCKDOWN, digits = 0)
      cases_full_lockdown_high_rr[ii,ncol(cases)+i] <- round(cases_full_lockdown_high_rr[ii,(ncol(cases)+i)-INCUBATION_TIME_DAYS] * BASIC_REPRODUCTION_NUMBER_HIGH * REPRODUCTION_RATE_DISCOUNT_LOCKDOWN, digits = 0)
    }
  }
  
  dummy <- data.frame(ageGroup = cases$ageGroup, totalCases = c(1:9), totalDeaths = c(1:9))
  
  total_cases_partial_lockdown_low_rr <- dummy
  total_cases_partial_lockdown_medium_rr <- dummy
  total_cases_partial_lockdown_high_rr <- dummy
  
  total_cases_full_lockdown_low_rr <- dummy
  total_cases_full_lockdown_medium_rr <- dummy
  total_cases_full_lockdown_high_rr <- dummy
  
  for (i in c(1:9)) {
    total_cases_partial_lockdown_low_rr[i,2] <- sum(cases_partial_lockdown_low_rr[i,13:(12+PERIOD_OF_INTEREST_DAYS)])
    total_cases_partial_lockdown_medium_rr[i,2] <- sum(cases_partial_lockdown_medium_rr[i,13:(12+PERIOD_OF_INTEREST_DAYS)])
    total_cases_partial_lockdown_high_rr[i,2] <- sum(cases_partial_lockdown_high_rr[i,13:(12+PERIOD_OF_INTEREST_DAYS)])
    
    total_cases_full_lockdown_low_rr[i,2] <- sum(cases_full_lockdown_low_rr[i,13:(12+PERIOD_OF_INTEREST_DAYS)])
    total_cases_full_lockdown_medium_rr[i,2] <- sum(cases_full_lockdown_medium_rr[i,13:(12+PERIOD_OF_INTEREST_DAYS)])
    total_cases_full_lockdown_high_rr[i,2] <- sum(cases_full_lockdown_high_rr[i,13:(12+PERIOD_OF_INTEREST_DAYS)])
  }
  
  
  ##########################################
  ## calculate deaths
  ##########################################
  partial_lockdown_low_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct *0.01, digits = 0))
  partial_lockdown_low_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct*0.01, digits = 0))
  partial_lockdown_low_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct*0.01, digits = 0))
  
  partial_lockdown_medium_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct*0.01, digits = 0))
  partial_lockdown_medium_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct*0.01, digits = 0))
  partial_lockdown_medium_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct*0.01, digits = 0))
  
  partial_lockdown_high_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct*0.01, digits = 0))
  partial_lockdown_high_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct*0.01, digits = 0))
  partial_lockdown_high_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct*0.01, digits = 0))
  
  
  full_lockdown_low_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct*0.01, digits = 0))
  full_lockdown_low_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct*0.01, digits = 0))
  full_lockdown_low_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct*0.01, digits = 0))
  
  full_lockdown_medium_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct*0.01, digits = 0))
  full_lockdown_medium_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct*0.01, digits = 0))
  full_lockdown_medium_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct*0.01, digits = 0))
  
  full_lockdown_high_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct*0.01, digits = 0))
  full_lockdown_high_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct*0.01, digits = 0))
  full_lockdown_high_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct*0.01, digits = 0))
  
  
  ##########################################
  ## calculate lost years
  ##########################################
  partial_lockdown_low_rr_low_fr$lostYears <- partial_lockdown_low_rr_low_fr$Deaths * lifeExp80$meanLifeExp
  partial_lockdown_low_rr_medium_fr$lostYears <- partial_lockdown_low_rr_medium_fr$Deaths * lifeExp80$meanLifeExp
  partial_lockdown_low_rr_high_fr$lostYears <- partial_lockdown_low_rr_high_fr$Deaths * lifeExp80$meanLifeExp
  
  partial_lockdown_medium_rr_low_fr$lostYears <- partial_lockdown_medium_rr_low_fr$Deaths * lifeExp80$meanLifeExp
  partial_lockdown_medium_rr_medium_fr$lostYears <- partial_lockdown_medium_rr_medium_fr$Deaths * lifeExp80$meanLifeExp
  partial_lockdown_medium_rr_high_fr$lostYears <- partial_lockdown_medium_rr_high_fr$Deaths * lifeExp80$meanLifeExp
  
  partial_lockdown_high_rr_low_fr$lostYears <- partial_lockdown_high_rr_low_fr$Deaths * lifeExp80$meanLifeExp
  partial_lockdown_high_rr_medium_fr$lostYears <- partial_lockdown_high_rr_medium_fr$Deaths * lifeExp80$meanLifeExp
  partial_lockdown_high_rr_high_fr$lostYears <- partial_lockdown_high_rr_high_fr$Deaths * lifeExp80$meanLifeExp
  
  
  full_lockdown_low_rr_low_fr$lostYears <- full_lockdown_low_rr_low_fr$Deaths * lifeExp80$meanLifeExp
  full_lockdown_low_rr_medium_fr$lostYears <- full_lockdown_low_rr_medium_fr$Deaths * lifeExp80$meanLifeExp
  full_lockdown_low_rr_high_fr$lostYears <- full_lockdown_low_rr_high_fr$Deaths * lifeExp80$meanLifeExp
  
  full_lockdown_medium_rr_low_fr$lostYears <- full_lockdown_medium_rr_low_fr$Deaths * lifeExp80$meanLifeExp
  full_lockdown_medium_rr_medium_fr$lostYears <- full_lockdown_medium_rr_medium_fr$Deaths * lifeExp80$meanLifeExp
  full_lockdown_medium_rr_high_fr$lostYears <- full_lockdown_medium_rr_high_fr$Deaths * lifeExp80$meanLifeExp
  
  full_lockdown_high_rr_low_fr$lostYears <- full_lockdown_high_rr_low_fr$Deaths * lifeExp80$meanLifeExp
  full_lockdown_high_rr_medium_fr$lostYears <- full_lockdown_high_rr_medium_fr$Deaths * lifeExp80$meanLifeExp
  full_lockdown_high_rr_high_fr$lostYears <- full_lockdown_high_rr_high_fr$Deaths * lifeExp80$meanLifeExp
  
  
  ##########################################
  ## calculate costs
  ##########################################
  partial_lockdown_low_rr_low_fr$costs <- partial_lockdown_low_rr_low_fr$lostYears * COST_LOST_YEAR
  partial_lockdown_low_rr_medium_fr$costs <- partial_lockdown_low_rr_medium_fr$lostYears * COST_LOST_YEAR
  partial_lockdown_low_rr_high_fr$costs <- partial_lockdown_low_rr_high_fr$lostYears * COST_LOST_YEAR
  
  partial_lockdown_medium_rr_low_fr$costs <- partial_lockdown_medium_rr_low_fr$lostYears * COST_LOST_YEAR
  partial_lockdown_medium_rr_medium_fr$costs <- partial_lockdown_medium_rr_medium_fr$lostYears * COST_LOST_YEAR
  partial_lockdown_medium_rr_high_fr$costs <- partial_lockdown_medium_rr_high_fr$lostYears * COST_LOST_YEAR
  
  partial_lockdown_high_rr_low_fr$costs <- partial_lockdown_high_rr_low_fr$lostYears * COST_LOST_YEAR
  partial_lockdown_high_rr_medium_fr$costs <- partial_lockdown_high_rr_medium_fr$lostYears * COST_LOST_YEAR
  partial_lockdown_high_rr_high_fr$costs <- partial_lockdown_high_rr_high_fr$lostYears * COST_LOST_YEAR
  
  
  full_lockdown_low_rr_low_fr$costs <- full_lockdown_low_rr_low_fr$lostYears * COST_LOST_YEAR
  full_lockdown_low_rr_medium_fr$costs <- full_lockdown_low_rr_medium_fr$lostYears * COST_LOST_YEAR
  full_lockdown_low_rr_high_fr$costs <- full_lockdown_low_rr_high_fr$lostYears * COST_LOST_YEAR
  
  full_lockdown_medium_rr_low_fr$costs <- full_lockdown_medium_rr_low_fr$lostYears * COST_LOST_YEAR
  full_lockdown_medium_rr_medium_fr$costs <- full_lockdown_medium_rr_medium_fr$lostYears * COST_LOST_YEAR
  full_lockdown_medium_rr_high_fr$costs <- full_lockdown_medium_rr_high_fr$lostYears * COST_LOST_YEAR
  
  full_lockdown_high_rr_low_fr$costs <- full_lockdown_high_rr_low_fr$lostYears * COST_LOST_YEAR
  full_lockdown_high_rr_medium_fr$costs <- full_lockdown_high_rr_medium_fr$lostYears * COST_LOST_YEAR
  full_lockdown_high_rr_high_fr$costs <- full_lockdown_high_rr_high_fr$lostYears * COST_LOST_YEAR
  
  
  ##########################################
  ## prepare output
  ##########################################
  scenario_r0 <- c("low","medium","high","low","medium","high","low","medium","high")
  scenario_fr <- c("low","low","low","medium","medium","medium","high","high","high")
  prior <- c(0.11,0.11,0.11,0.11,0.11,0.11,0.11,0.11,0.11)
  health_cost_fld <- c(
    sum(full_lockdown_low_rr_low_fr$costs),
    sum(full_lockdown_medium_rr_low_fr$costs),
    sum(full_lockdown_high_rr_low_fr$costs),
    sum(full_lockdown_low_rr_medium_fr$costs),
    sum(full_lockdown_medium_rr_medium_fr$costs),
    sum(full_lockdown_high_rr_medium_fr$costs),
    sum(full_lockdown_low_rr_high_fr$costs),
    sum(full_lockdown_medium_rr_high_fr$costs),
    sum(full_lockdown_high_rr_high_fr$costs)
  )
  
  health_cost_pld <- c(
    sum(partial_lockdown_low_rr_low_fr$costs),
    sum(partial_lockdown_medium_rr_low_fr$costs),
    sum(partial_lockdown_high_rr_low_fr$costs),
    sum(partial_lockdown_low_rr_medium_fr$costs),
    sum(partial_lockdown_medium_rr_medium_fr$costs),
    sum(partial_lockdown_high_rr_medium_fr$costs),
    sum(partial_lockdown_low_rr_high_fr$costs),
    sum(partial_lockdown_medium_rr_high_fr$costs),
    sum(partial_lockdown_high_rr_high_fr$costs)
  )
  
  output <- data.frame(scenario_r0, scenario_fr, prior, health_cost_fld, health_cost_pld)
  return(output)
}

# Function to calculate the death cases----------
get_deaths <- function(
  INCUBATION_TIME_DAYS = 10,
  REPRODUCTION_RATE_DISCOUNT_LOCKDOWN = .45,
  BASIC_REPRODUCTION_NUMBER_LOW = 1.4,
  BASIC_REPRODUCTION_NUMBER_MEDIUM = 2.79,
  BASIC_REPRODUCTION_NUMBER_HIGH = 3.3,
  PERIOD_OF_INTEREST_DAYS = 33
) {
  ##########################################
  ## import data
  ##########################################
  load("lifeExpectancy.Rdata")
  load("caseFatalityGrouped.RData")
  load("cases.RData")
  
  
  ##########################################
  ## calculate cases
  ##########################################
  cases_partial_lockdown_low_rr <- cases
  cases_partial_lockdown_medium_rr <- cases
  cases_partial_lockdown_high_rr <- cases
  
  cases_full_lockdown_low_rr <- cases
  cases_full_lockdown_medium_rr <- cases
  cases_full_lockdown_high_rr <- cases
  
  for (i in c(1:PERIOD_OF_INTEREST_DAYS)) {
    for (ii in c(1:9)) {
      cases_partial_lockdown_low_rr[ii,ncol(cases)+i] <- round(cases_partial_lockdown_low_rr[ii,(ncol(cases)+i)-INCUBATION_TIME_DAYS] * BASIC_REPRODUCTION_NUMBER_LOW, digits = 0)
      cases_partial_lockdown_medium_rr[ii,ncol(cases)+i] <- round(cases_partial_lockdown_medium_rr[ii,(ncol(cases)+i)-INCUBATION_TIME_DAYS] * BASIC_REPRODUCTION_NUMBER_MEDIUM, digits = 0)
      cases_partial_lockdown_high_rr[ii,ncol(cases)+i] <- round(cases_partial_lockdown_high_rr[ii,(ncol(cases)+i)-INCUBATION_TIME_DAYS] * BASIC_REPRODUCTION_NUMBER_HIGH, digits = 0)
      
      cases_full_lockdown_low_rr[ii,ncol(cases)+i] <- round(cases_full_lockdown_low_rr[ii,(ncol(cases)+i)-INCUBATION_TIME_DAYS] * BASIC_REPRODUCTION_NUMBER_LOW * REPRODUCTION_RATE_DISCOUNT_LOCKDOWN, digits = 0)
      cases_full_lockdown_medium_rr[ii,ncol(cases)+i] <- round(cases_full_lockdown_medium_rr[ii,(ncol(cases)+i)-INCUBATION_TIME_DAYS] * BASIC_REPRODUCTION_NUMBER_MEDIUM * REPRODUCTION_RATE_DISCOUNT_LOCKDOWN, digits = 0)
      cases_full_lockdown_high_rr[ii,ncol(cases)+i] <- round(cases_full_lockdown_high_rr[ii,(ncol(cases)+i)-INCUBATION_TIME_DAYS] * BASIC_REPRODUCTION_NUMBER_HIGH * REPRODUCTION_RATE_DISCOUNT_LOCKDOWN, digits = 0)
    }
  }
  
  dummy <- data.frame(ageGroup = cases$ageGroup, totalCases = c(1:9), totalDeaths = c(1:9))
  
  total_cases_partial_lockdown_low_rr <- dummy
  total_cases_partial_lockdown_medium_rr <- dummy
  total_cases_partial_lockdown_high_rr <- dummy
  
  total_cases_full_lockdown_low_rr <- dummy
  total_cases_full_lockdown_medium_rr <- dummy
  total_cases_full_lockdown_high_rr <- dummy
  
  for (i in c(1:9)) {
    total_cases_partial_lockdown_low_rr[i,2] <- sum(cases_partial_lockdown_low_rr[i,13:(12+PERIOD_OF_INTEREST_DAYS)])
    total_cases_partial_lockdown_medium_rr[i,2] <- sum(cases_partial_lockdown_medium_rr[i,13:(12+PERIOD_OF_INTEREST_DAYS)])
    total_cases_partial_lockdown_high_rr[i,2] <- sum(cases_partial_lockdown_high_rr[i,13:(12+PERIOD_OF_INTEREST_DAYS)])
    
    total_cases_full_lockdown_low_rr[i,2] <- sum(cases_full_lockdown_low_rr[i,13:(12+PERIOD_OF_INTEREST_DAYS)])
    total_cases_full_lockdown_medium_rr[i,2] <- sum(cases_full_lockdown_medium_rr[i,13:(12+PERIOD_OF_INTEREST_DAYS)])
    total_cases_full_lockdown_high_rr[i,2] <- sum(cases_full_lockdown_high_rr[i,13:(12+PERIOD_OF_INTEREST_DAYS)])
  }
  
  
  ##########################################
  ## calculate deaths
  ##########################################
  partial_lockdown_low_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct*0.01, digits = 0))
  partial_lockdown_low_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct*0.01, digits = 0))
  partial_lockdown_low_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct*0.01, digits = 0))
  
  partial_lockdown_medium_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct*0.01, digits = 0))
  partial_lockdown_medium_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct*0.01, digits = 0))
  partial_lockdown_medium_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct*0.01, digits = 0))
  
  partial_lockdown_high_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct*0.01, digits = 0))
  partial_lockdown_high_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct*0.01, digits = 0))
  partial_lockdown_high_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct*0.01, digits = 0))
  
  
  full_lockdown_low_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct*0.01, digits = 0))
  full_lockdown_low_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct*0.01, digits = 0))
  full_lockdown_low_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct*0.01, digits = 0))
  
  full_lockdown_medium_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct*0.01, digits = 0))
  full_lockdown_medium_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct*0.01, digits = 0))
  full_lockdown_medium_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct*0.01, digits = 0))
  
  full_lockdown_high_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct*0.01, digits = 0))
  full_lockdown_high_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct*0.01, digits = 0))
  full_lockdown_high_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct*0.01, digits = 0))
  
  
  # Aggregate death cases and generate output
  partial_lockdown_deaths = join_all(list(partial_lockdown_low_rr_low_fr, partial_lockdown_low_rr_medium_fr, partial_lockdown_low_rr_high_fr, partial_lockdown_medium_rr_low_fr, partial_lockdown_medium_rr_medium_fr,
                                          partial_lockdown_medium_rr_high_fr, partial_lockdown_high_rr_low_fr, partial_lockdown_high_rr_medium_fr, partial_lockdown_high_rr_high_fr), by = "ageGroup", type = "left")
  
  partial_lockdown_deaths$mean = NA
  
  for(i in 1:nrow(partial_lockdown_deaths)){
    partial_lockdown_deaths$mean[i] = round(mean(as.numeric(partial_lockdown_deaths[i,2:10])),0)
  }
  
  
  full_lockdown_deaths = join_all(list(full_lockdown_low_rr_low_fr, full_lockdown_low_rr_medium_fr, full_lockdown_low_rr_high_fr, full_lockdown_medium_rr_low_fr, full_lockdown_medium_rr_medium_fr,
                                       full_lockdown_medium_rr_high_fr, full_lockdown_high_rr_low_fr, full_lockdown_high_rr_medium_fr, full_lockdown_high_rr_high_fr), by = "ageGroup", type = "left")
  
  for(i in 1:nrow(full_lockdown_deaths)){
    full_lockdown_deaths$mean[i] = round(mean(as.numeric(full_lockdown_deaths[i,2:10])),0)
  }
  
  output = data.frame(ageGroup = full_lockdown_deaths$ageGroup, FullLockdown = full_lockdown_deaths$mean, PartialLockdown = partial_lockdown_deaths$mean)
  
  output1 = melt(output, id.vars = "ageGroup")
  
  return(output1)
}

# Import data---------
load("PUB_Data.Rdata")
load("Tax_Data.Rdata")

# Define state variable
Acts = c("Full Lockdown", "Partial Lockdown")
Health_Prior = c(1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9)

# server--------------
# Define server logic
shinyServer(function(input, output, session) {
  
  # Receive Inputs----------
  COST_LOST_YEAR = reactive({input$costPerYear})
  utilityType = reactive({input$utilityType})
  t_Ld = reactive({input$t_Ld})
  d_CV = reactive({input$d_CV})
  CV_SME = reactive({input$CV_SME})
  CV_LE = reactive({input$CV_LE})
  OEE_FL = reactive({input$OEE_FL})
  p_pessimistic = reactive({input$p_pessimistic})
  p_neutral = reactive({input$p_neutral})
  p_optimistic = reactive({1-p_pessimistic()-p_neutral()})
  
  observe({
    p_pess = input$p_pessimistic
    updateSliderInput(session = session, inputId = "p_neutral", min = 0, max = 1-p_pess, value = (1-p_pess)/2, step = 0.001)})
  
  INCUBATION_TIME_DAYS = reactive({input$INCUBATION_TIME_DAYS})
  BASIC_REPRODUCTION_NUMBER_LOW = reactive({input$BASIC_REPRODUCTION_NUMBER_LOW})
  BASIC_REPRODUCTION_NUMBER_MEDIUM = reactive({input$BASIC_REPRODUCTION_NUMBER_MEDIUM})
  BASIC_REPRODUCTION_NUMBER_HIGH = reactive({input$BASIC_REPRODUCTION_NUMBER_HIGH})
  REPRODUCTION_RATE_DISCOUNT_LOCKDOWN = reactive({1-input$REPRODUCTION_RATE_DISCOUNT_LOCKDOWN})
  
  # Bring Economic and Health costs together
  EconCost = reactive({EconCostCalc(PUB_Data = PUB_Data, Tax_Data = Tax_Data, t_Ld = t_Ld()/30, d_CV = d_CV(), CV_SME = CV_SME(), CV_LE = CV_LE(), OEE_FL = OEE_FL(),
                                     p_pessimistic = p_pessimistic(), p_neutral = p_neutral(), p_optimistic = p_optimistic())})
  HealthCost = reactive({get_health_costs(INCUBATION_TIME_DAYS = INCUBATION_TIME_DAYS(), BASIC_REPRODUCTION_NUMBER_LOW = BASIC_REPRODUCTION_NUMBER_LOW(),
                                                            BASIC_REPRODUCTION_NUMBER_MEDIUM = BASIC_REPRODUCTION_NUMBER_MEDIUM(), BASIC_REPRODUCTION_NUMBER_HIGH = BASIC_REPRODUCTION_NUMBER_HIGH(),
                                                            REPRODUCTION_RATE_DISCOUNT_LOCKDOWN = REPRODUCTION_RATE_DISCOUNT_LOCKDOWN(), COST_LOST_YEAR = COST_LOST_YEAR(), PERIOD_OF_INTEREST_DAYS = t_Ld())})
  
  SEU_TotalCostStates = reactive({gatherCostHealth(
    # Economic Variables
    EconCostScenario = EconCost()$EconShockExp, EconCostPrior = EconCost()$Prior, EconCostLD = EconCost()$EconCostsLd, EconCostNM = EconCost()$EconCostsNoLd,
    # Health Variables
    HealthScenarioR = HealthCost()$scenario_r0, HealthScenarioCFR = HealthCost()$scenario_fr, HealthPrior = Health_Prior, HealthCostsLD = HealthCost()$health_cost_fld, HealthCostsNM = HealthCost()$health_cost_pld)})
  
  # Calculate expected utility based on inputs
  FL_ExpUtility = reactive({
    if(utilityType()=="neutral"){
      sum(SEU_TotalCostStates()$Prior*linearUtility(EconCost = SEU_TotalCostStates()$EconCostLD, HealthCost = SEU_TotalCostStates()$HealthCostsLD))}
    else{
      sum(SEU_TotalCostStates()$Prior*quadraticUtility(EconCost = SEU_TotalCostStates()$EconCostLD, HealthCost = SEU_TotalCostStates()$HealthCostsLD))
    }
  })
  
  PL_ExpUtility = reactive({
    if(utilityType()=="neutral"){
      sum(SEU_TotalCostStates()$Prior*linearUtility(EconCost = SEU_TotalCostStates()$EconCostNM, HealthCost = SEU_TotalCostStates()$HealthCostsNM))}
    else{
      sum(SEU_TotalCostStates()$Prior*quadraticUtility(EconCost = SEU_TotalCostStates()$EconCostNM, HealthCost = SEU_TotalCostStates()$HealthCostsNM))
    }
  })
  
  ExpUtility = reactive({c(FL_ExpUtility(), PL_ExpUtility())})
  
  # Calculate expected costs based on inputs
  FL_ExpEconCost = reactive({sum(SEU_TotalCostStates()$Prior*SEU_TotalCostStates()$EconCostLD)})
  
  PL_ExpEconCost = reactive({sum(SEU_TotalCostStates()$Prior*SEU_TotalCostStates()$EconCostNM)})
  
  ExpEconCost = reactive({c(FL_ExpEconCost(), PL_ExpEconCost())})
  
  # Calculate expected death cases
  FL_ExpDeaths = reactive({sum(deathCasesGrouped()$FullLockdown)})

  PL_ExpDeaths = reactive({sum(deathCasesGrouped()$PartialLockdown)})

  ExpDeaths = reactive({c(FL_ExpDeaths(), PL_ExpDeaths())})
  
  # Calculate expected death cases
  deathCases = reactive({get_deaths(INCUBATION_TIME_DAYS = INCUBATION_TIME_DAYS(), BASIC_REPRODUCTION_NUMBER_LOW = BASIC_REPRODUCTION_NUMBER_LOW(),
                          BASIC_REPRODUCTION_NUMBER_MEDIUM = BASIC_REPRODUCTION_NUMBER_MEDIUM(), BASIC_REPRODUCTION_NUMBER_HIGH = BASIC_REPRODUCTION_NUMBER_HIGH(),
                          REPRODUCTION_RATE_DISCOUNT_LOCKDOWN = REPRODUCTION_RATE_DISCOUNT_LOCKDOWN(), PERIOD_OF_INTEREST_DAYS = t_Ld())})
  
  deathCasesGrouped = reactive({
    spread(deathCases(), variable, value)
  })
  
  # create a dataframe for all plots
  plotData = reactive({data.frame(Acts=Acts, ExpUtility=ExpUtility(), ExpEconCost=ExpEconCost(), ExpDeaths = ExpDeaths())})
  
  # Plot the expected utility
  offsetU = reactive({abs(min(plotData()$ExpUtility))*1.4})
  SEU_plot = reactive({
    ggplot(plotData(), aes(x=reorder(Acts,ExpUtility), y=ExpUtility+offsetU())) +
    geom_bar(stat="identity", fill="darkblue") +
    coord_flip() +
    geom_text(aes(label = round(ExpUtility,3)), size=5, hjust=1.2, colour = "white", fontface=2) +
    labs(x=NULL, y=NULL) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(size=20,face="bold"),
          line = element_blank(),
          panel.background = element_blank())})
  
  output$SEUplot = renderPlot({SEU_plot()})
  
  # Plot the expected costs
  offsetC = reactive({abs(min(plotData()$ExpEconCost))*0})
  ExpCost_Plot = reactive({ggplot(plotData(), aes(x=reorder(Acts,-ExpEconCost), y=ExpEconCost+offsetC())) +
      geom_bar(stat="identity", fill="darkred") +
      coord_flip() +
      geom_text(aes(label = round(ExpEconCost/1000000000,3)), size=5, hjust=1.2, colour = "white", fontface=2) +
      labs(x=NULL, y=NULL) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y = element_text(size=20,face="bold"),
            line = element_blank(),
            panel.background = element_blank())})
  
  output$ExpCostPlot = renderPlot({ExpCost_Plot()})
  
  # Plot the expected total death cases
  offsetD = reactive({abs(max(plotData()$ExpDeaths))*0.1})
  ExpDeath_Plot = reactive({
    ggplot(plotData(), aes(x=reorder(Acts,-ExpDeaths), y=ExpDeaths+offsetD())) +
      geom_bar(stat="identity", fill="black") +
      coord_flip() +
      geom_text(aes(label = ExpDeaths), size=5, hjust=1.2, colour = "white", fontface=2) +
      labs(x=NULL, y=NULL) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y = element_text(size=20,face="bold"),
            line = element_blank(),
            panel.background = element_blank())})
  
  output$ExpDeathPlot = renderPlot({ExpDeath_Plot()})
  
  # Plot the expected death cases by age group
  ExpDeathByAge_Plot = reactive({
    ggplot(deathCases(), aes(x=ageGroup, y=value, fill=variable)) +
      geom_bar(stat="identity", position = "dodge") +
      coord_flip() +
      labs(x="Age Group", y="Number of cases", fill="Scenario") +
      scale_x_continuous(limits = c(-5,85), breaks = c(10*0:8)) +
      theme_classic()
  })
  
  output$ExpDeathByAgePlot = renderPlot({ExpDeathByAge_Plot()})
  
  # Render Table for Economic and Health costs
  output$TotalCostStates = renderTable({
    SEU_TotalCostStates()
  }, options=list(searching=FALSE, paging=TRUE), rownames=FALSE, filter="top")

})
