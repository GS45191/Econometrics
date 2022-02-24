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

# First, load df in WDI_FP.CPI

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("WDI: Inflation Analysis"),
    
    sidebarLayout(
        sidebarPanel(

        selectInput(
            inputId = "country",
            label = "Choose country:",
            choices = countrycode::codelist$country.name.en,
            multiple = TRUE
        ),
            
        selectInput(
            inputId = "startyear",
            label = "Choose starting year:",
            choices = min(df$year):max(df$year),
            selected = min(df$year)
        ),
        
        selectInput(
            inputId = "endyear",
            label = "Choose ending year:",
            choices = min(df$year):max(df$year),
            selected = max(df$year)
        )),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("cpi_plot"),
           tableOutput("cpi_table")
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
            scale_x_continuous(breaks = seq(input$startyear, 
                                            input$endyear, 
                                            by = 1)) +
            scale_y_continuous(name = "Inflation, consumer prices (annual %)", 
                               labels = scales::comma) +
            theme_minimal()
    })
    
    output$cpi_table <- renderTable({
        
        # Table
            df %>% 
            filter(year > input$startyear & year < input$endyear,
                   country %in% input$country) %>% 
            select(-iso2c) %>% 
            group_by(year, country) %>% 
            arrange(by_group = year) %>% 
            pivot_wider(names_from = year,
                        values_from = cpi,
                        values_fn = ~sum(.x, na.rm = TRUE))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
