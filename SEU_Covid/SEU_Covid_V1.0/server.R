# Packages--------------
library(ggplot2)
library(tidyverse)
library(shiny)

# load functions--------------
load("../Functions/f_cost.R")
load("../Functions/f_linearUtility.R")
load("../Functions/f_quadraticUtility.R")

# Import data---------
load("../Data/stateData.Rdata")

# Define state variable
Acts = c("Full Lockdown", "Partial Lockdown", "No measures")

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
  
  # Calculate costs based on inputs
  FL_cost = reactive({cost(unemployed = stateData$FL_unemployment, unemployedExp = unemployedExp(),
                 workbase = workbase(), wage = wage(), months = stateData$FL_duration,
                 subventions = stateData$FL_subventions, defaultRate = stateData$FL_default,
                 creditVolume = stateData$FL_credits)})
  
  PL_cost = reactive({cost(unemployed = stateData$PL_unemployment, unemployedExp = unemployedExp(),
                 workbase = workbase(), wage = wage(), months = stateData$PL_duration,
                 subventions = stateData$PL_subventions, defaultRate = stateData$PL_default,
                 creditVolume = stateData$PL_credits)})
  
  NM_cost = reactive({cost(unemployed = stateData$NM_unemployment, unemployedExp = unemployedExp(),
                 workbase = workbase(), wage = wage(), months = stateData$NM_duration,
                 subventions = stateData$NM_subventions, defaultRate = stateData$NM_default,
                 creditVolume = stateData$NM_credits)})
  
  # Calculate expected utility based on inputs
  FL_ExpUtility = reactive({
    if(utilityType()=="linear"){
      sum(stateData$priors*linearUtility(Cost=FL_cost(), Deaths = stateData$FL_deaths, CostPerDeath = costPerDeath()))}
    else{
      sum(stateData$priors*quadraticUtility(Cost=FL_cost(), Deaths = stateData$FL_deaths, CostPerDeath = costPerDeath()))
    }
  })
  
  PL_ExpUtility = reactive({
    if(utilityType()=="linear"){
      sum(stateData$priors*linearUtility(Cost=PL_cost(), Deaths = stateData$PL_deaths, CostPerDeath = costPerDeath()))}
    else{
      sum(stateData$priors*quadraticUtility(Cost=PL_cost(), Deaths = stateData$PL_deaths, CostPerDeath = costPerDeath()))
    }
  })
  
  NM_ExpUtility = reactive({
    if(utilityType()=="linear"){
      sum(stateData$priors*linearUtility(Cost=NM_cost(), Deaths = stateData$NM_deaths, CostPerDeath = costPerDeath()))}
    else{
      sum(stateData$priors*quadraticUtility(Cost=NM_cost(), Deaths = stateData$NM_deaths, CostPerDeath = costPerDeath()))
    }
  })
  
  ExpUtility = reactive({c(FL_ExpUtility(), PL_ExpUtility(), NM_ExpUtility())})
  
  # Calculate expected costs based on inputs
  FL_ExpCost = reactive({sum(stateData$priors*FL_cost())})
  
  PL_ExpCost = reactive({sum(stateData$priors*PL_cost())})
  
  NM_ExpCost = reactive({sum(stateData$priors*NM_cost())})
  
  ExpCost = reactive({c(FL_ExpCost(), PL_ExpCost(), NM_ExpCost())})
  
  # Calculate expected death cases
  FL_ExpDeaths = sum(stateData$priors*stateData$FL_deaths)
  
  PL_ExpDeaths = sum(stateData$priors*stateData$PL_deaths)
  
  NM_ExpDeaths = sum(stateData$priors*stateData$NM_deaths)
  
  ExpDeaths = c(FL_ExpDeaths, PL_ExpDeaths, NM_ExpDeaths)
  
  # create a dataframe for all plots
  plotData = reactive({data.frame(Acts=Acts, ExpUtility=ExpUtility(), ExpCost=ExpCost(), ExpDeaths=ExpDeaths)})
  
  # Plot the expected utility
  offsetU = reactive({abs(min(plotData()$ExpUtility))*1.4})
  SEU_plot = reactive({ggplot(plotData(), aes(x=reorder(Acts,ExpUtility), y=ExpUtility+offsetU())) +
    geom_bar(stat="identity", fill="darkblue") +
    coord_flip() +
    geom_text(aes(label = round(ExpUtility,0)), size=5, hjust=1.2, colour = "white", fontface=2) +
    labs(x=NULL, y=NULL) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(size=20,face="bold"),
          line = element_blank(),
          panel.background = element_blank())})
  
  output$SEUplot = renderPlot({SEU_plot()})
  
  # Plot the expected costs
  offsetC = reactive({abs(min(plotData()$ExpCost))*0})
  ExpCost_Plot = reactive({ggplot(plotData(), aes(x=reorder(Acts,-ExpCost), y=ExpCost+offsetC())) +
      geom_bar(stat="identity", fill="darkred") +
      coord_flip() +
      geom_text(aes(label = round(ExpCost/1000000000,3)), size=5, hjust=1.2, colour = "white", fontface=2) +
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

})
