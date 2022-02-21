#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(WDI)
library(tidyverse)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("WDI: Prices-Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("year",
            #             "Choose a year:",
            #             min = min(df$year),
            #             max = max(df$year),
            #             value = 2018,
            #             step = 1,
            #             dragRange = TRUE)
            
        selectInput(
            inputId = "country",
            label = "Choose country:",
            choices = df$country,
            multiple = TRUE
        ),
            
        selectInput(
            inputId = "startyear",
            label = "Choose startin year:",
            choices = min(df$year):max(df$year),
            selected = min(df$year)
        ),
        
        selectInput(
            inputId = "endyear",
            label = "Choose ending year:",
            choices = min(df$year):max(df$year),
            selected = max(df$year)
        )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("cpi_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$cpi_plot <- renderPlot({
        # draw the histogram with the specified number of bins
            df %>% 
            filter(year > input$startyear & year < input$endyear,
                   country %in% input$country) %>% 
            ggplot(aes(x = year, 
                       y = cpi, 
                       color = country)) +
            geom_point() +
            geom_line() +
            theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
