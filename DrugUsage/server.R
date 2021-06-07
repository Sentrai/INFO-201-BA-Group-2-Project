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
        "This project is designed to visualize drug use (e.g., alcohol, 
        cigarettes, marijuana, etc.) by age in the United States. It provides 
        three interactive pages that cover drug usage by state, usage rate over 
        time by age, and the age at which someone is first exposed to a drug. 
        These visualizations allow healthcare providers to more easily explain 
        how and when drug use occurs in a given location to their clients to 
        target potential misconceptions about drug usage. This might guide 
        providers as to where more information about drugs is most relevant. 
        The data used in these visualizations is provided by the National Survey 
        on Drug Use and Health using data from 1999-2018 and 2015-2019. "
        
    })
    
    output$coverImage <- renderImage({
        filename <- normalizePath(file.path('Cover.jpg'))
        list(src = filename,
             alt = paste("Cover Image", input$n))
    }, deleteFile = FALSE)


    
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
        "This chart shows drug usage by the state for a specified drug, year, 
        and age group. Ideally, this could better inform lawmakers and 
        institutions about where more information on drugs is most relevant and 
        inform healthcare providers in some states about what drug-related 
        topics they should cover with their patients and at what ages."
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
        "This bar chart, which purely focuses on age and substance type, would 
        be most useful for healthcare providers and academic institutions. The 
        chart could allow these groups to better understand the demographics 
        they are working with and the information most relevant to that group. 
        For example, in recent years, inhalants have skewed much younger than 
        cocaine. "
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
        "This chart, which shows drug usage by an age group over time (meaning 
        the number of people in a given age group who used a specified drug for 
        a given time), might help policymakers better understand the impact 
        that their policies and messaging have had on drug usage. This also 
        shows various trends for certain drugs over time, such as with marijuana 
        which has seen a dramatic rise in the past decade. This information
        would be useful for healthcare providers trying to better understand 
        the demographics whom they serve."
    })
    # CONCLUSION
    output$notableinsightone <- renderText({
        "One notable insight discovered in our project was that the average age 
        when first using for hard subtances such as cocaine, heroin, LSD, and 
        hallucinogens is around 20 years old. The average 
        age stayed consistent from 2015-2019. When choosing the different 
        substances and changing the year you can see how the graphs are
        positively skewed." })
    
    output$graphImage1 <- renderImage({
        filename <- normalizePath(file.path('graph_1.png'))
        list(src = filename,
             width = 500, height = 300,
             alt = paste("Graph_1", input$n))
    }, deleteFile = FALSE)
    
    output$graphImage2 <- renderImage({
        filename <- normalizePath(file.path('graph_2.png'))
        list(src = filename,
             width = 500, height = 300,
             alt = paste("Graph_2", input$n))
    }, deleteFile = FALSE)
    
    output$cocaine <- renderImage({
        filename <- normalizePath(file.path('cocaine.png'))
        list(src = filename,
             width = 500, height = 300,
             alt = paste("cocaine", input$n))
    }, deleteFile = FALSE)
    
    output$lsd <- renderImage({
        filename <- normalizePath(file.path('lsd.png'))
        list(src = filename,
             width = 500, height = 300,
             alt = paste("lsd", input$n))
    }, deleteFile = FALSE)
    
    output$heroin <- renderImage({
        filename <- normalizePath(file.path('heroin.png'))
        list(src = filename,
             width = 500, height = 300,
             alt = paste("Gheroin", input$n))
    }, deleteFile = FALSE)
    
    output$hallucinogin <- renderImage({
        filename <- normalizePath(file.path('hallucinogin.png'))
        list(src = filename,
             width = 500, height = 300,
             alt = paste("hallucinogin", input$n))
    }, deleteFile = FALSE)
        
    output$notableinsighttwo <- renderText({
        "The second notable insight discovered in our project 
        was that cocaine usage and mental health services received for the age 
        group 18 and older both have a postive trend line from 2010-2019. 
        While we cannot assume directly that the two are correlated,
        it is interesting to see how more people are getting 
        mental health services and that more people are also using cocaine."
    })
    
    output$broaderandquality <- renderText({
        "The broader implications of the insight show that 
        most people start to experiment with hard drugs is around 
        the age of 20. This could be due to easier access in college and having 
        more financial independence. The quality of the data 
        was not perfect as there were gaps for certain years regarding the Drug
        Misuse and the Mental Health graph. There were also some years of 
        data not available for the Drug Use Rate Over Time by Age Group. 
        The data was also collected by offering a $30 incentive to participants. 
        This makes the results biased towards people of lower income versus 
        people of higher income that would not care too much about a 
        $30 incentive. An idea to advance this project in the 
        future would be to compare drug usage in states based on income."
        
        
        
    })

    
})
