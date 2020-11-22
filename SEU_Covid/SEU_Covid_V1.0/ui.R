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
                                      
                                      withMathJax(includeMarkdown("Description.md"))
                                 
                             ),
                             
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
                                        sidebarPanel(p(strong("Utility function")),
                                                     selectInput("utilityType","Type of utility function", choices=c("linear","quadratic"), selected = "linear"),
                                                     br(),
                                                     p(strong("Economic factors")),
                                                     sliderInput("costPerYear", "Cost per lost year of lifetime", 25000, 500000, step=5000, value=100000),
                                                     sliderInput("meanWage", "Mean wage before unemployment", 4000, 10000, step=1, value=6656),
                                                     sliderInput("workbase", "Workbase in Switzerland", 3000000, 6000000, step=10000, value=4230000),
                                                     sliderInput("unemploymentExp", "Expected Unemployment (without Covid)", 0, 1, step=0.001, value=0.023)
                                                     ),
                                        
                                        # Main-Panel for the results
                                        mainPanel(
                                          tabsetPanel(
                                            
                                            # Sub-Tab for assessing the cost
                                            tabPanel("Utility",
                                                     br(),
                                                     p("Savage Expected Utility, based on all assumtions and factors."),
                                                     plotOutput("SEUplot")
                                                     ),
                                            tabPanel("Economic Impact",
                                                     br(),
                                                     p("Expected economic impact in terms of governmental costs, based on all assumtions and factors. In Billion CHF."),
                                                     plotOutput("ExpCostPlot")
                                                     ),
                                            tabPanel("Health Impact",
                                                     br(),
                                                     p("Expected health impact in terms of death cases, based on all assumtions and factors."),
                                                     plotOutput("ExpDeathPlot")
                                                     )
                                          )
                                        )
                                      )
                                      
                                      
                                      
                             )
                  )
))
