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
        tabPanel(
            "Introduction",
                mainPanel(
                   uiOutput("introduction"),
                   imageOutput("coverImage")
            )
        ),
        
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
                    plotOutput("distPlot"),
                    uiOutput("summary1")
                )
            )
        ),
        tabPanel("Drug Use Rate Over Time by Age Group",
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("category2"),
                         uiOutput("agegrp2")
                     ),
                     mainPanel(
                         plotOutput("linePlot"),
                         uiOutput("summary2")
                     )
                 )),
        tabPanel("Age When First Use of Drug",
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("substance"),
                         uiOutput("year2")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("barPlot"),
                         uiOutput("summary3")
                     )
                 )),
        tabPanel(
            "Conclusion",
            mainPanel(
                uiOutput("conclusion") 
            )
        )
    

    
    )
))
