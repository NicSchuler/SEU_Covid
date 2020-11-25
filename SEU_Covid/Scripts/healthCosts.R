# Script to calculate the health costs
library(tidyr)


##########################################
## import data
##########################################
load("Data/lifeExpectancy.RData")
load("Data/caseFatalityGrouped.RData")
load("Data/cases.RData")

INCUBATION_TIME_DAYS <- 10
REPRODUCTION_RATE_DISCOUNT_LOCKDOWN <- .45

BASIC_REPRODUCTION_NUMBER_LOW <- 1.4
BASIC_REPRODUCTION_NUMBER_MEDIUM <- 2.79
BASIC_REPRODUCTION_NUMBER_HIGH <- 3.3

PERIOD_OF_INTEREST_DAYS <- 33


##########################################
## calculate cases
##########################################
cases_partial_lockdown_low_rr <- cases
cases_partial_lockdown_medium_rr <- cases
cases_partial_lockdown_high_rr <- cases

cases_full_lockdown_low_rr <- cases
cases_full_lockdown_medium_rr <- cases
cases_full_lockdown_high_rr <- cases

for (i in c(1:33)) {
  for (ii in c(1:9)) {
    cases_partial_lockdown_low_rr[ii,ncol(cases)+i] <- round(cases_partial_lockdown_low_rr[ii,(ncol(cases)+i)-10] * BASIC_REPRODUCTION_NUMBER_LOW, digits = 0)
    cases_partial_lockdown_medium_rr[ii,ncol(cases)+i] <- round(cases_partial_lockdown_medium_rr[ii,(ncol(cases)+i)-10] * BASIC_REPRODUCTION_NUMBER_MEDIUM, digits = 0)
    cases_partial_lockdown_high_rr[ii,ncol(cases)+i] <- round(cases_partial_lockdown_high_rr[ii,(ncol(cases)+i)-10] * BASIC_REPRODUCTION_NUMBER_HIGH, digits = 0)
    
    cases_full_lockdown_low_rr[ii,ncol(cases)+i] <- round(cases_full_lockdown_low_rr[ii,(ncol(cases)+i)-10] * BASIC_REPRODUCTION_NUMBER_LOW * REPRODUCTION_RATE_DISCOUNT_LOCKDOWN, digits = 0)
    cases_full_lockdown_medium_rr[ii,ncol(cases)+i] <- round(cases_full_lockdown_medium_rr[ii,(ncol(cases)+i)-10] * BASIC_REPRODUCTION_NUMBER_MEDIUM * REPRODUCTION_RATE_DISCOUNT_LOCKDOWN, digits = 0)
    cases_full_lockdown_high_rr[ii,ncol(cases)+i] <- round(cases_full_lockdown_high_rr[ii,(ncol(cases)+i)-10] * BASIC_REPRODUCTION_NUMBER_HIGH * REPRODUCTION_RATE_DISCOUNT_LOCKDOWN, digits = 0)
  }
}

dummy <- data.frame(cases$ageGroup, c(1:9), c(1:9))
colnames(dummy) <- c("ageGroup", "totalCases", "totalDeaths")

total_cases_partial_lockdown_low_rr <- dummy
total_cases_partial_lockdown_medium_rr <- dummy
total_cases_partial_lockdown_high_rr <- dummy

total_cases_full_lockdown_low_rr <- dummy
total_cases_full_lockdown_medium_rr <- dummy
total_cases_full_lockdown_high_rr <- dummy

for (i in c(1:9)) {
  total_cases_partial_lockdown_low_rr[i,2] <- sum(cases_partial_lockdown_low_rr[i,13:45])
  total_cases_partial_lockdown_medium_rr[i,2] <- sum(cases_partial_lockdown_medium_rr[i,13:45])
  total_cases_partial_lockdown_high_rr[i,2] <- sum(cases_partial_lockdown_high_rr[i,13:45])
  
  total_cases_full_lockdown_low_rr[i,2] <- sum(cases_full_lockdown_low_rr[i,13:45])
  total_cases_full_lockdown_medium_rr[i,2] <- sum(cases_full_lockdown_medium_rr[i,13:45])
  total_cases_full_lockdown_high_rr[i,2] <- sum(cases_full_lockdown_high_rr[i,13:45])
}


##########################################
## calculate deaths
##########################################
partial_lockdown_low_rr_low_fr <- data.frame(cases$ageGroup, round(total_cases_partial_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityLowPrct, digits = 0))
partial_lockdown_low_rr_medium_fr <- data.frame(cases$ageGroup, round(total_cases_partial_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityMediumPrct, digits = 0))
partial_lockdown_low_rr_high_fr <- data.frame(cases$ageGroup, round(total_cases_partial_lockdown_low_rr$totalCases * caseFatalityGrouped$caseFatalityHighPrct, digits = 0))

partial_lockdown_medium_rr_low_fr
partial_lockdown_medium_rr_medium_fr
partial_lockdown_medium_rr_high_fr

partial_lockdown_high_rr_low_fr
partial_lockdown_high_rr_medium_fr
partial_lockdown_high_rr_high_fr


full_lockdown_low_rr_low_fr
full_lockdown_low_rr_medium_fr
full_lockdown_low_rr_high_fr

full_lockdown_medium_rr_low_fr
full_lockdown_medium_rr_medium_fr
full_lockdown_medium_rr_high_fr

full_lockdown_high_rr_low_fr
full_lockdown_high_rr_medium_fr
full_lockdown_high_rr_high_fr

