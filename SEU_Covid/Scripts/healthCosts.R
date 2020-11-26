# Script to calculate the health costs
library(tidyr)

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
    load("Data/lifeExpectancy.RData")
    load("Data/caseFatalityGrouped.RData")
    load("Data/cases.RData")
    
    
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

