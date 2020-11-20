# Packages--------
library(shiny)
library(tidyverse)
library(shinythemes)
library(knitr)

# Import markdowns-------
rmdfiles <- c("../Description.Rmd")
sapply(rmdfiles, knit, quiet = T)

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
                             tabPanel("Model Inputs", h1("Redefine the model inputs and assumptions"), br()
                                 
                                 
                             ),
                             
                             # third tab "Decision making"--------
                             tabPanel("Decision making", h1("See the optimal decision"), br()
                                      
                             )
                  )
))
