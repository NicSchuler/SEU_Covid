# linear utility function----------
# define a function to calculate the linear utility of a scenario
linearUtility = function(Cost, Deaths, CostPerDeath){
  utility = -(Cost/CostPerDeath + Deaths)
  return(utility)
}

# quadratic utility function----------
# define a function to calculate the quadratic utility of a scenario
quadraticUtility = function(Cost, Deaths, CostPerDeath){
  utility = -(Cost/CostPerDeath + Deaths)^2
  return(utility)
}

# cost function------------
cost = function(unemployed, unemployedExp, workbase, wage, months, subventions, defaultRate, creditVolume){
  costs = (unemployed-unemployedExp)*workbase*0.8*wage*months + subventions + defaultRate*creditVolume
  return(costs)
}

# save the functions-----------
save(linearUtility, file="Functions/f_linearUtility.R")
save(quadraticUtility, file="Functions/f_quadraticUtility.R")
save(cost, file="Functions/f_cost.R")
