# packages
library(tidyverse)

# Preparation of BVG_Table------------
# load the BVG-Table from 2005
# BVG=read.delim("BVG_table2005.CSV", sep=";")

# remove rows for age 17 - 19 (for better clusters)
# BVG1 = BVG %>% 
#   filter(age>=20) %>% 
#   filter(age<90)

# create a vector for age clusters
age80 = 10*c(0:8)

# create dataframe for the life expectancy based on the age cluster
# lifeExp80 = data.frame(age = age80, meanLifeExp=NA)

# calculate the mean life expectancy for each cluster
# for (i in 3:9){
#   min = (i-1)*10
#   max = (i*10)-1
#   temp = BVG1 %>% 
#     filter(age>= min) %>% 
#     filter(age<=max)
#   meanlifeMan = mean(temp$e_man)
#   meanlifeWoman = mean(temp$e_woman)
#   lifeExp80[i,2] = (meanlifeMan + meanlifeWoman)/2
# }

# impute missing values (based on internet resources)
# lifeExp80[1,2] = 77.361
# lifeExp80[2,2] = 67.373

# Import finished mean life expectancy table
load("Data/lifeExpectancy.Rdata")


# "Import" data from China CDC Weekly and build dataframe with
# confirmed cases, deaths and case fatality rate per age cluster
cnConfCases = c(416,549,3619,7600,8571,10008,8583,3918,1408)
cnDeaths = c(0,1,7,18,38,130,309,312,208)

ChinaData = data.frame(age=age80, cnConfCases, cnDeaths)

ChinaData$caseFatality = round(ChinaData$cnDeaths / ChinaData$cnConfCases,6)
ChinaData$caseAgePerc = round(ChinaData$cnConfCases / sum(ChinaData$cnConfCases),6)

# bring all data together
mortalityData = full_join(lifeExp80, ChinaData, by="age")

# calculate the average lost years by a covid-death
avLostYears = sum(mortalityData$meanLifeExp*mortalityData$cnDeaths)/sum(mortalityData$cnDeaths)

# Define cost per lost year
costPerLostYear = 222000

# Calculate average cost per covid-death
avCostPerDeath = costPerLostYear*avLostYears

# 1'689'656 CHF if 100'000 per Year
# 3'751'035 CHF if 222'000 per Year

# save(mortalityData, file="Data/mortalityData.Rdata")

