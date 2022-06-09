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
library(shinyjs)


con_db <- dbConnect(RSQLite::SQLite(), "db/log.db")



future::plan(strategy = future::multisession)



panel_sim_1 <- uiOutput(
  outputId = "sim_1"
)





# Define UI for application that draws a histogram
ui <- fluidPage(

    useShinyjs(),

    # Application titl
    titlePanel("Old Faithful Geyser Data"),

    tabsetPanel(
      tabPanel(
        title = "Simulation",
        sidebarLayout(
          sidebarPanel = sidebarPanel(
            width = 4,
            splitLayout(
              panel_sim_1
            )

          ),
          mainPanel = mainPanel(

          )
        )
      ) ,
      tabPanel(
        title = "Game"
      )
    )


)

# Define server logic required to draw a histogram
server <- function(input, output) {


    output$sim_1 <- renderUI({
      saida <- create_inputs("1")

      browser()

      saida

    })


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
