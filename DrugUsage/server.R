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
    
    barDf <- read_csv("nsduh_2015-2019.csv") %>%
        rename(
            "/CIGTRY" = "CIGTRY",
            "/ALCTRY" = "ALCTRY",
            "/MJAGE" = "MJAGE",
            "/COCAGE" = "COCAGE",
            "/HERAGE" = "HERAGE",
            "/HALLUCAGE" = "HALLUCAGE",
            "/LSDAGE" = "LSDAGE",
            "/INHALAGE" = "INHALAGE"
        ) %>% 
        pivot_longer(
            contains("/"),
            names_to = "substance",
            values_to = 'age'
        ) %>% 
        mutate(substance = gsub("/", "", substance))
    
    # INTRODUCTION
    output$introduction <- renderText({
        "Write text here."
    })
    
    # CHART 1 WORK
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
    
    output$summary1 <- renderText ({
        "Write text here."
    })
    
    # CHART 3 WORK
    output$substance <- renderUI ({
        radioButtons("substance", label = h3("Select Substance Type"), 
                     choices = list("Cigarettes" = "CIGTRY", "Alchohol"= "ALCTRY", "Marijuana" = "MJAGE",
                                    "Cocaine" = "COCAGE", "Heroin" = "HERAGE", "Hallucinogens" = "HALLUCAGE",
                                    "LSD" = "LSDAGE", "Inhalants" = "INHALAGE"),
                     selected = "CIGTRY")
    })
    
    output$year2 <- renderUI({
        sliderInput("year2", label = h3("Year"), min = 2015, 
                    max = 2019, value = 2019, step = 1)
    })
    
    
    barSample <- reactive({
        barDf %>%
            filter(Year == input$year2) %>%
            filter(substance == input$substance) %>%
                filter(age < 80) 
                    
                    
    })
    
    output$barPlot <- renderPlot({
        ggplot(barSample(), aes(x = age)) +
            geom_bar(fill = "grey")
    })
    
    output$summary3 <- renderText ({
        "Write text here."
    })
    
    # CHART 2 WORK
    lineData <- reactive({
        eg <-  data %>%
            filter(agegrp == input$age2) %>%
            filter(outname == input$category2) %>%
            filter(region == "national")
        
        validate(
            need(nrow(eg) != 0, "Data not available. Please change age group or year.")
        )
        
        data %>%
            filter(agegrp == input$age2) %>%
            filter(outname == input$category2) %>%
            filter(region == "national")
        
    })
    
    #chart 2 line plot output
    output$linePlot <- renderPlot({
        ggplot(lineData(), aes(x = year, y = BSAE, color = agegrp)) +
            geom_line() +
            geom_point() +
            labs(title = "Drugs BSAE from 1999-2019 for different Age Groups", 
                 x = "Year",
                 y = "BSAE",
                 color = "Age Group"
            ) 
        
        
    })
    
    output$category2 <- renderUI({
        selectInput("category2", label = h3("Drug Usage or Mental Health Category"),
                    choices = unique(data$outname), 
                    selected = "alcohol use in the past month")
    })
    
    output$agegrp2 <- renderUI ({
        radioButtons("age2", label = h3("Select Age Group"), 
                     choices = list("12 or older" = 0, "12 to 17" = 1, "18 to 25" = 2,
                                    "26 or older" = 3, "18 or older" = 4), 
                     selected = 4)
    })
    
    output$summary2 <- renderText ({
        "Write text here."
    })
    # CONCLUSION
    output$conclusion <- renderText({
        "A notable insight discovered in our project was that the average age 
        when first using for all substances is around 20 years old. The average 
        age stayed consistent from 2015-2019. The chart the demonstrates the 
        pattern is the Age When First Use of Drug. When choosing the different 
        substances and changing the year, you can see how the graph is 
        positively skewed. The broader implications of the insight show that 
        most people start to experiment with different kinds of drug at around 
        the age of 20. This could be due to easier access in college and the 
        legalization of certain drugs such as marijuana.The quality of the data 
        is satisfactory as it did not have any gaps and had data for all 48 
        contiguous states. However, the data was collected by offering a $30 
        incentive to participants. This makes the results biased towards people 
        of lower income versus people of higher income that would not care too
        much about a $30 incentive. An idea to advance this project in the 
        future would be to compare drug usage in states based on income."
    })
    
})
