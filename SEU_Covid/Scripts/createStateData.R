# Create a Vector for our states
states = c("pessimistic", "normal", "optimistic")

# create a Vector for the priors
priors = c(1/3, 1/3, 1/3)

# create the data points (FL=Full Lockdown, PL=Partial Lockdown, NM=No measures)
# unemployment
FL_unemployment = c(0.5,0.3,0.1)
PL_unemployment = c(0.4,0.2,0.1)
NM_unemployment = c(0.3,0.2,0.1)

# duration of unemployment (in months)
FL_duration = c(6, 4, 2)
PL_duration = c(5, 3, 1)
NM_duration = c(4, 2, 0)

# payed subventions
FL_subventions = c(1000000000, 500000000, 100000000)
PL_subventions = c(800000000, 400000000, 80000000)
NM_subventions = c(600000000, 300000000, 60000000)

# credit default rate
FL_default = c(0.01, 0.005, 0.001)
PL_default = c(0.008, 0.004, 0.0008)
NM_default = c(0.006, 0.003, 0.0006)

# credit volume
FL_credits = c(10000000000, 5000000000, 1000000000)
PL_credits = c(8000000000, 4000000000, 800000000)
NM_credits = c(6000000000, 3000000000, 600000000)

# death cases
FL_deaths = c(10000,5000,1000)
PL_deaths = c(20000,10000,2000)
NM_deaths = c(100000,50000,10000)

# bring it all together in one data frame
stateData = data.frame(states, priors, FL_unemployment, PL_unemployment, NM_unemployment,
                       FL_duration, PL_duration, NM_duration, FL_subventions, PL_subventions, NM_subventions,
                       FL_default, PL_default, NM_default, FL_credits, PL_credits, NM_credits,
                       FL_deaths, PL_deaths, NM_deaths)

# save the data
# save(stateData, file="Data/stateData.Rdata")
