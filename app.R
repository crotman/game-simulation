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

panel_sim_2 <- uiOutput(
  outputId = "sim_2"
)




# Define UI for application that draws a histogram
ui <- fluidPage(

    useShinyjs(),

    # Application titl
    titlePanel("How are you Kludging?"),

    tabsetPanel(
      tabPanel(
        title = "Simulation",
        sidebarLayout(
          sidebarPanel = sidebarPanel(
            width = 4,
            splitLayout(
              panel_sim_1,
              panel_sim_2
            ),
            actionButton(
              inputId = "btn_simulate",
              label = "Simulate!"
            )
          ),
          mainPanel = mainPanel(
            reactableOutput(outputId = "saida")
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

      saida

    })


    output$sim_2 <- renderUI({
      saida <- create_inputs("2")

      saida

    })


    resultado <- eventReactive(
      eventExpr = input$btn_simulate,
      valueExpr = {
        simulate_action_button(input)
      }
    )


    output$saida <- renderReactable({

      resultado <- resultado()

      execucao_1 <- resultado$result_1 %>%
        filter(
          str_detect(key, "developer")
        ) %>%
        group_by(
          value
        ) %>%
        summarise(
          merges_payoff = n()/5
        ) %>%
        pivot_wider(
          names_from = value,
          values_from = merges_payoff
        ) %>%
        mutate(
          case = "Case 1",
          .before = everything()
        )

      execucao_2 <- resultado$result_2 %>%
        filter(
          str_detect(key, "developer")
        ) %>%
        group_by(
          value
        ) %>%
        summarise(
          merges_payoff = n()/5
        ) %>%
        pivot_wider(
          names_from = value,
          values_from = merges_payoff
        ) %>%
        mutate(
          case = "Case 2",
          .before = everything()
        )



      reactable(
        bind_rows(execucao_1, execucao_2)
      )


    })






}

# Run the application
shinyApp(ui = ui, server = server)
