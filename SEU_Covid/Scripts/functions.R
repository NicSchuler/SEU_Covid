# linear utility function----------
# define a function to calculate the linear utility of a scenario
linearUtility = function(EconCost, HealthCost){
  utility = round(-(EconCost + HealthCost)/1000000000,3)
  return(utility)
}

# quadratic utility function----------
# define a function to calculate the quadratic utility of a scenario
quadraticUtility = function(EconCost, HealthCost){
  utility = round(-((EconCost + HealthCost)/1000000000)^2,3)
  return(utility)
}

# cost function------------
cost = function(unemployed, unemployedExp, workbase, wage, months, subventions, defaultRate, creditVolume){
  costs = (unemployed-unemployedExp)*workbase*0.8*wage*months + subventions + defaultRate*creditVolume
  return(costs)
}

# gather data function------
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

# save the functions-----------
save(linearUtility, file="Functions/f_linearUtility.R")
save(quadraticUtility, file="Functions/f_quadraticUtility.R")
save(cost, file="Functions/f_cost.R")
save(gatherCostHealth, file="Functions/f_gatherCostHealth.R")
