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
                h3("Notable Insight One"),
                uiOutput("notableinsightone"),
                h5("Cocaine"),
                imageOutput("cocaine"),
                h5("LSD"),
                imageOutput("lsd"),
                h5("Heroin"),
                imageOutput("heroin"),
                h5("Hallucinogins"),
                imageOutput("hallucinogin"),
                h3("Notable Insight Two"),
                uiOutput("notableinsighttwo"),
                h5("Cocaine Use in the Past Year"),
                imageOutput("graphImage1"),
                h5("Recieved Mental Health Services"),
                imageOutput("graphImage2"),
                h3("Broader Implications and Quality of Data"),
                uiOutput("broaderandquality")
                
            
            )
        )
    

    
    )
))
