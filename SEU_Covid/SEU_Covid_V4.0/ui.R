# Packages--------
library(shiny)
library(tidyverse)
library(shinythemes)



# publish app
# rsconnect::deployApp()

# UI--------
# Define UI for application
shinyUI(fluidPage(theme=shinytheme("yeti"),
                  
                  # Initialize tab-layout
                  navbarPage(title="Savage Expected Utility",
                             
                             # main tab "Decision making"--------
                             tabPanel("Decision making",
                                      h1("Assess the optimal decision"),
                                      p("based on variable inputs, which can be set on the left hand side."),
                                      
                                      # initialize Sidebar-Layout
                                      sidebarLayout(
                                        # Sidebar-Panel for setting the Parameters
                                        sidebarPanel(p(strong("Change Parameters")),
                                                     selectInput("utilityType","Risk-profile", choices=c("neutral","averse"), selected = "neutral"),
                                                     p(strong("General factors")),
                                                     flowLayout(sliderInput("costPerYear", "Cost per lost year of lifetime", 25000, 500000, step=5000, value=100000),
                                                     sliderInput("t_Ld", "Lockdown Duration in days", 0, 180, step=1, value=33)),
                                                     p(strong("Economic factors")),
                                                     flowLayout(sliderInput("d_CV", "Credit default rate", 0, 1, step=0.001, value=0.05),
                                                     sliderInput("CV_SME", "Credit volume for SME", 0, 100000000000, step=1000000000, value=20000000000),
                                                     sliderInput("CV_LE", "Credit volume for LE", 0, 100000000000, step=1000000000, value=20000000000),
                                                     sliderInput("OEE_FL", "Other extraordinary expenses", 0, 10000000000, step=1000000000, value=4000000000),
                                                     sliderInput("p_pessimistic", "Prior for pessimistic scenario", 0, 1, step=0.001, value=1/3),
                                                     sliderInput("p_neutral", "Prior for neutral scenario", 0, 1, step=0.001, value=1/3)),
                                                     p(strong("Health factors")),
                                                     flowLayout(sliderInput("INCUBATION_TIME_DAYS", "Transmission time", 1, 10, step=1, value=9),
                                                     sliderInput("BASIC_REPRODUCTION_NUMBER_LOW", "R0 optimistic", 1, 5, step=0.01, value=1.4),
                                                     sliderInput("BASIC_REPRODUCTION_NUMBER_MEDIUM", "R0 medium", 1, 5, step=0.01, value=2.79),
                                                     sliderInput("BASIC_REPRODUCTION_NUMBER_HIGH", "R0 pessimistic", 1, 5, step=0.01, value=3.3),
                                                     sliderInput("REPRODUCTION_RATE_DISCOUNT_LOCKDOWN", "R0 discount through Lockdown", 0, 1, step=0.01, value=0.55))
                                                     ),
                                        
                                        # Main-Panel for the results
                                        mainPanel(
                                          # Initialize Sub-Tabs for decision making--------
                                          tabsetPanel(
                                            
                                            # Sub-Tab for utility-----------
                                            tabPanel("Utility",
                                                     br(),
                                                     p("Savage Expected Utility, based on all assumtions and factors."),
                                                     plotOutput("SEUplot")
                                                     ),
                                            # Sub-Tab for Economic Cost--------
                                            tabPanel("Economic Impact",
                                                     br(),
                                                     p("Expected economic impact in terms of governmental costs, based on all assumtions and factors. In Billion CHF."),
                                                     plotOutput("ExpCostPlot")
                                                     ),
                                            # Sub-Tab for Health-Cost---------
                                            tabPanel("Health Impact",
                                                     br(),
                                                     p("Expected health impact in terms of total death cases, based on all assumtions and factors."),
                                                     plotOutput("ExpDeathPlot"),
                                                     br(),
                                                     p("Expected health impact in terms of death cases by age group, based on all assumtions and factors."),
                                                     plotOutput("ExpDeathByAgePlot")
                                                     ),
                                            # Sub-Tab for States
                                            tabPanel("States",
                                                     br(),
                                                     p("All states based on all assumtions and factors."),
                                                     tableOutput("TotalCostStates"))
                                          )
                                        )
                                      )
                                      
                                      
                                      
                             )
                  )
))
