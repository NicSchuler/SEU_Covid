# Packages--------------
library(ggplot2)
library(tidyverse)
library(shiny)

# load functions--------------
load("../Functions/f_cost.R")
load("../Functions/f_linearUtility.R")
load("../Functions/f_quadraticUtility.R")
load("../Functions/f_gatherCostHealth.R")

# Import data---------
load("../Data/stateData.Rdata")
load("../Data/EconomicCosts.RData")
load("../Data/Test_TotalCost.RData")

# Define state variable
Acts = c("Full Lockdown", "Partial Lockdown")

# server--------------
# Define server logic
shinyServer(function(input, output) {
  
  # Tables for model inputs------
  # Table: Unemployment and its duration
  output$stateUnempDuration = renderTable({
    stateData[,c(1:8)] 
  }, options=list(searching=FALSE, paging=TRUE), rownames=FALSE, filter="top")
  
  # Table: Subventions
  output$stateSubventions = renderTable({
    stateData[,c(1,2,9:11)] 
  }, options=list(searching=FALSE, paging=TRUE), rownames=FALSE, filter="top")
  
  # Table: Credit default rate and credit volume
  output$stateCredits = renderTable({
    stateData[,c(1,2,12:17)] 
  }, options=list(searching=FALSE, paging=TRUE), rownames=FALSE, filter="top")
  
  # Table: death cases
  output$stateDeaths = renderTable({
    stateData[,c(1,2,18:20)] 
  }, options=list(searching=FALSE, paging=TRUE), rownames=FALSE, filter="top")
  
  # Decision making-----------
  # Receive Inputs
  costPerDeath = reactive({16.8965557184751*input$costPerYear})
  wage = reactive({input$meanWage})
  workbase = reactive({input$workbase})
  unemployedExp = reactive({input$unemploymentExp})
  utilityType = reactive({input$utilityType})
  
  # Bring Economic and Health costs together
  EconCost = EconCost
  HealthCost = Test_HC
  SEU_TotalCostStates = gatherCostHealth(
    # Economic Variables
    EconCostScenario = EconCost$EconShockExp, EconCostPrior = EconCost$Prior, EconCostLD = EconCost$EconCostsLd, EconCostNM = EconCost$EconCostsNoLd,
    # Health Variables
    HealthScenarioR = HealthCost$Scenario_R0, HealthScenarioCFR = HealthCost$Scenario_CFR, HealthPrior = HealthCost$Prior, HealthCostsLD = HealthCost$HealthCost_LD, HealthCostsNM = HealthCost$HealthCost_NM)
  
  # Calculate expected utility based on inputs
  FL_ExpUtility = reactive({
    if(utilityType()=="linear"){
      sum(SEU_TotalCostStates$Prior*linearUtility(EconCost = SEU_TotalCostStates$EconCostLD, HealthCost = SEU_TotalCostStates$HealthCostsLD))}
    else{
      sum(SEU_TotalCostStates$Prior*quadraticUtility(EconCost = SEU_TotalCostStates$EconCostLD, HealthCost = SEU_TotalCostStates$HealthCostsLD))
    }
  })
  
  PL_ExpUtility = reactive({
    if(utilityType()=="linear"){
      sum(SEU_TotalCostStates$Prior*linearUtility(EconCost = SEU_TotalCostStates$EconCostNM, HealthCost = SEU_TotalCostStates$HealthCostsNM))}
    else{
      sum(SEU_TotalCostStates$Prior*quadraticUtility(EconCost = SEU_TotalCostStates$EconCostNM, HealthCost = SEU_TotalCostStates$HealthCostsNM))
    }
  })
  
  ExpUtility = reactive({c(FL_ExpUtility(), PL_ExpUtility())})
  
  # Calculate expected costs based on inputs
  FL_ExpEconCost = sum(SEU_TotalCostStates$Prior*SEU_TotalCostStates$EconCostLD)
  
  PL_ExpEconCost = sum(SEU_TotalCostStates$Prior*SEU_TotalCostStates$EconCostNM)
  
  ExpEconCost = c(FL_ExpEconCost, PL_ExpEconCost)
  
  # Calculate expected death cases
  FL_ExpDeaths = sum(SEU_TotalCostStates$Prior*SEU_TotalCostStates$FL_deaths)
  
  PL_ExpDeaths = sum(SEU_TotalCostStates$Prior*SEU_TotalCostStates$PL_deaths)
  
  ExpDeaths = c(FL_ExpDeaths, PL_ExpDeaths)
  
  # create a dataframe for all plots
  plotData = reactive({data.frame(Acts=Acts, ExpUtility=ExpUtility(), ExpEconCost=ExpEconCost, ExpDeaths=ExpDeaths)})
  
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
  
  # Plot the expected death cases
  offsetD = reactive({abs(min(plotData()$ExpDeaths))*0})
  ExpDeath_Plot = reactive({ggplot(plotData(), aes(x=reorder(Acts,-ExpDeaths), y=ExpDeaths+offsetD())) +
      geom_bar(stat="identity", fill="black") +
      coord_flip() +
      geom_text(aes(label = round(ExpDeaths,0)), size=5, hjust=1.2, colour = "white", fontface=2) +
      labs(x=NULL, y=NULL) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y = element_text(size=20,face="bold"),
            line = element_blank(),
            panel.background = element_blank())})
  
  output$ExpDeathPlot = renderPlot({ExpDeath_Plot()})
  
  # Render Table for Economic and Health costs
  output$TotalCostStates = renderTable({
    SEU_TotalCostStates
  }, options=list(searching=FALSE, paging=TRUE), rownames=FALSE, filter="top")

})
