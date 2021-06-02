#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    navbarPage( "Drug Misuse and Mental Health",
        tabPanel(# Sidebar with a slider input for number of bins
            "Drug Use Prevalance by State",
            sidebarLayout(
                sidebarPanel(
                   uiOutput("category"),
                   uiOutput("year"),
                   uiOutput("agegrp")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                    plotOutput("distPlot")
                )
            )
        ),
        tabPanel("Chart2"),
        tabPanel("Chart3")
       
    

    
    )
))
