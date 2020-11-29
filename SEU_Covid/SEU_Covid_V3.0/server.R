# Packages--------------
library(plyr)
library(ggplot2)
library(tidyverse)
library(shiny)
library(reshape2)
library(rsconnect)

# load own functions--------------
load("f_linearUtility.R")
load("f_quadraticUtility.R")
load("f_gatherCostHealth.R")
load("f_EconCostCalc.R")
load("f_get_health_costs.R")
load("f_get_deaths.R")

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
  REPRODUCTION_RATE_DISCOUNT_LOCKDOWN = reactive({input$REPRODUCTION_RATE_DISCOUNT_LOCKDOWN})
  
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
    if(utilityType()=="linear"){
      sum(SEU_TotalCostStates()$Prior*linearUtility(EconCost = SEU_TotalCostStates()$EconCostLD, HealthCost = SEU_TotalCostStates()$HealthCostsLD))}
    else{
      sum(SEU_TotalCostStates()$Prior*quadraticUtility(EconCost = SEU_TotalCostStates()$EconCostLD, HealthCost = SEU_TotalCostStates()$HealthCostsLD))
    }
  })
  
  PL_ExpUtility = reactive({
    if(utilityType()=="linear"){
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
