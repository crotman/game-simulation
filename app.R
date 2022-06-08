#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(simmer)
library(shiny)
library(truncnorm)
library(shiny)
library(reactable)
library(patchwork)
library(rhandsontable)
library(scales)
library(ggforce)
library(here)
library(waiter)
library(shinydashboardPlus)
library(ggrepel)
library(slider)
library(memoise)
library(DBI)
library(shiny)
library(tidyverse)



con_db <- dbConnect(RSQLite::SQLite(), "db/log.db")



future::plan(strategy = future::multisession)


likert_harm <- tibble(
    entropy = c(1, 1.0025, 1.005, 1.0075, 1.01, 1.0125, 1.015, 1.0175, 1.02),
    description = c("1", "1.0025", "1.005", "1.0075", "1.01", "1.0125", "1.015", "1.0175", "1.02")
)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
