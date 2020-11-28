# Script to calculate the health costs
library(tidyr)
library(plyr)
library(reshape2)

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
  load("../Data/lifeExpectancy.RData")
  load("../Data/caseFatalityGrouped.RData")
  load("../Data/cases.RData")
  
  
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
  partial_lockdown_low_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct, digits = 0))
  partial_lockdown_low_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct, digits = 0))
  partial_lockdown_low_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct, digits = 0))
  
  partial_lockdown_medium_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct, digits = 0))
  partial_lockdown_medium_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct, digits = 0))
  partial_lockdown_medium_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct, digits = 0))
  
  partial_lockdown_high_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct, digits = 0))
  partial_lockdown_high_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct, digits = 0))
  partial_lockdown_high_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_partial_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct, digits = 0))
  
  
  full_lockdown_low_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct, digits = 0))
  full_lockdown_low_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct, digits = 0))
  full_lockdown_low_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct, digits = 0))
  
  full_lockdown_medium_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct, digits = 0))
  full_lockdown_medium_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct, digits = 0))
  full_lockdown_medium_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_medium_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct, digits = 0))
  
  full_lockdown_high_rr_low_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct, digits = 0))
  full_lockdown_high_rr_medium_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct, digits = 0))
  full_lockdown_high_rr_high_fr <- data.frame(ageGroup = cases$ageGroup, Deaths = round(total_cases_full_lockdown_high_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct, digits = 0))
  
  
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

save(get_deaths, file="Functions/f_get_deaths.R")