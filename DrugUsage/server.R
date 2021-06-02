#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(maps)
library(tidyverse)
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    data <- read_csv("NSDUH_data_1999-2018.csv")
    
    us <- map_data("state")
    
    sample <- reactive({
        eg <-  data %>%
            filter(year == input$year) %>%
            filter(agegrp == input$age) %>%
            filter(outname == input$category)
        
        validate(
            need(nrow(eg) != 0, "Data not available. Please change age group or year.")
                )
            
        data %>%
                filter(year == input$year) %>%
                filter(agegrp == input$age) %>%
                filter(outname == input$category) %>%
                left_join(us, data, by = "region")
        
    })
    output$distPlot <- renderPlot({
        
       ggplot(sample(), aes(x = long, y = lat, group=group, fill = BSAE)) +
            geom_polygon(col = "grey") +
            ggtitle(str_to_title(paste(input$category, "in the USA in", input$year))) + 
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank()) + 
            coord_quickmap()
        

    })
    
    output$agegrp <- renderUI ({
        radioButtons("age", label = h3("Select Age Group"), 
                    choices = list("12 or older" = 0, "12 to 17" = 1, "18 to 25" = 2,
                                  "26 or older" = 3, "18 or older" = 4), 
                    selected = 4)
    })
    
    output$year <- renderUI({
        sliderInput("year", label = h3("Year"), min = 1999, 
                    max = 2018, value = 2018, step = 1)
    })
    
    output$category <- renderUI({
        selectInput("category", label = h3("Drug Usage or Mental Health Category"),
                     choices = unique(data$outname), 
                     selected = "alcohol use in the past month")
    })

})
