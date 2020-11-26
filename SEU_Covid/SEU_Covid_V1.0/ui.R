# Packages--------
library(shiny)
library(tidyverse)
library(shinythemes)
library(knitr)

# Import and wrangle markdown-------
rmdfiles <- c("../Description.Rmd")
sapply(rmdfiles, knit, quiet = T)

# Import data---------
load("../Data/stateData.Rdata")

# UI--------
# Define UI for application
shinyUI(fluidPage(theme=shinytheme("yeti"),
                  
                  # Initialize tab-layout
                  navbarPage(title="Savage Expected Utility",
                             
                             # first tab "Explanations"--------
                             tabPanel("Explanations", h1("Explanation of the methodology"), br(),
                                      tabsetPanel(
                                        tabPanel("Savage Expected Utility",
                                      
                                        withMathJax(includeMarkdown("Description.md"))),
                                        
                                        tabPanel("Economic Model"),
                                        
                                        tabPanel("Health Model"),
                                        
                                        tabPanel("Utility function"),
                                        
                                        tabPanel("Underlying assumptions")
                                      
                                      
                                 
                             )),
                             
                             # second tab "Model inputs"--------
                             tabPanel("Model Inputs",
                                      
                                      # Header
                                      h1("View the model inputs and assumptions"),
                                      p("FL refers to full lockdown, PL to partial lockdown and NM to no measures in the variable's suffixes"),
                                      br(),
                                      
                                      # first sub-tab "Economic Inputs"--------
                                      tabsetPanel(tabPanel("Economic Inputs",
                                        h2("Economic inputs"),
                                        p(strong("Unemployment and its duration")),
                                        tableOutput("stateUnempDuration"), 
                                        br(),
                                        p(strong("Subventions")),
                                        tableOutput("stateSubventions"),
                                        br(),
                                        p(strong("Credit defaults")),
                                        tableOutput("stateCredits")
                                      ),
                                      
                                      # second sub-tab "Health Inputs"--------
                                      tabPanel("Health inputs",
                                        h2("Health inputs"),
                                        p(strong("Death cases")),
                                        tableOutput("stateDeaths")
                                      )
                             )),
                             
                             # third tab "Decision making"--------
                             tabPanel("Decision making",
                                      h1("Assess the optimal decision"),
                                      p("based on variable inputs, which can be set on the left hand side."),
                                      
                                      # initialize Sidebar-Layout
                                      sidebarLayout(
                                        # Sidebar-Panel for setting the Parameters
                                        sidebarPanel(p(strong("Change Parameters")),
                                                     selectInput("utilityType","Type of utility function", choices=c("linear","quadratic"), selected = "linear"),
                                                     p(strong("General factors")),
                                                     sliderInput("costPerYear", "Cost per lost year of lifetime", 25000, 500000, step=5000, value=100000),
                                                     sliderInput("t_Ld", "Lockdown Duration in days", 0, 180, step=1, value=33),
                                                     p(strong("Economic factors")),
                                                     sliderInput("d_CV", "Credit default rate", 0, 1, step=0.001, value=0.05),
                                                     sliderInput("CV_SME", "Credit volume for SME", 0, 100000000000, step=1000000000, value=20000000000),
                                                     sliderInput("CV_LE", "Credit volume for LE", 0, 100000000000, step=1000000000, value=20000000000),
                                                     sliderInput("OEE_FL", "Other extraordinary expenses", 0, 100000000000, step=1000000000, value=20000000000),
                                                     sliderInput("p_pessimistic", "Prior for pessimistic scenario", 0, 1, step=0.001, value=1/3),
                                                     sliderInput("p_neutral", "Prior for neutral scenario", 0, 1, step=0.001, value=1/3)
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
                                                     p("Expected health impact in terms of death cases, based on all assumtions and factors."),
                                                     plotOutput("ExpDeathPlot")
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
