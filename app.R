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
library(slider)

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
            numericInput(inputId = "n_executions", value = 5, label = "# Executions", width = "200px"),
            actionButton(
              inputId = "btn_simulate",
              label = "Simulate!"
            )
          ),
          mainPanel = mainPanel(
            reactableOutput(outputId = "saida"),
            tags$hr(),
            plotOutput(outputId = "evolution")
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
        progress <- shiny::Progress$new()
        on.exit(progress$close())

        results_pre <- simulate_action_button(input, progress)


        results <- bind_rows(
          results_pre$result_1 %>%  mutate(case = "Case 1"),
          results_pre$result_2 %>%  mutate(case = "Case 2")
        )


        stages <- tibble::tribble(
          ~id, ~name, ~phase,
          1,  "Development", "Start",
          2,  "Development", "End",
          3,  "Review", "Start",
          4,  "Review", "End",
          5,  "MetaReview", "Start",
          6,  "MetaReview", "End",
          7,  "Merge", "Start",
          8,  "Merge", "End",
        )



        data <- results %>%
          filter(
            str_detect(string = key, pattern = "pr[0-9]*")
          ) %>%
          mutate(
            pullrequest = str_extract(string = key, pattern = "pr[0-9]*"),
            field = str_extract(string = key, pattern = "(?:_).*") %>% str_remove("_"),
            .before = value
          ) %>%
          dplyr::select(-key) %>%
          tidyr::pivot_wider(
            names_from = field,
            values_from = value
          ) %>%
          dplyr::arrange(time) %>%
          dplyr::group_by(pullrequest, replication, round) %>%
          tidyr::fill(
            dplyr::everything(),
            .direction = "down"
          ) %>%
          dplyr::ungroup() %>%
          dplyr::left_join(
            stages %>% dplyr::rename(cur_stage = name),
            by = c("stage" = "id")
          ) %>%
          dplyr::mutate(
            dplyr::across(
              .cols = where(is.numeric) & !time,
              .fns = as.integer
            )
          )


        data


      }
    )


    output$evolution <- renderPlot({

      resultado <- resultado()


      n_executions <- max(resultado$round)

      results_final <- resultado %>%
        filter(
          cur_stage == "Merge", phase == "End"
        ) %>%
        arrange(time) %>%
        group_by(developer, case) %>%
        mutate(
          cumul_merges = cumsum(!is.na(developer))/n_executions,
          last_merges = slide_index_dbl(
            .i = time,
            .x = !is.na(developer),
            .f = sum,
            .before = 1000
          )/n_executions
        ) %>%
        filter(
          time >= 1000
        )


      ggplot(results_final) +
        ggtitle(
          "Merges during last 1000 time units"
        ) +
        geom_line(
          aes(
            x = time,
            y = last_merges
          )
        ) +
        facet_grid(
          case ~ developer
        ) +
        theme_minimal()







    })

    output$saida <- renderReactable({


      resultado <- resultado()

      n_executions <- max(resultado$round)

      results_final <- resultado %>%
        filter(
          cur_stage == "Merge", phase == "End"
        ) %>%
        group_by(
          developer, case
        ) %>%
        summarise(
          merges = n()/n_executions
        ) %>%
        pivot_wider(
          names_from = developer,
          names_prefix = "Developer ",
          values_from = merges
        )


      final_table <- bind_rows(results_final) %>%
        rowwise() %>%
        mutate(
          total = sum(c_across(cols = starts_with("Developer")))
        ) %>%
        ungroup()


      reactable(
        final_table,

        columns = list(
          case = colDef(
            name = "Case"
          ),
          total = colDef(
            name = "Total"
          )
        ),

        defaultColDef = colDef(
          format = colFormat(
            digits = 1
          )
        )
      )


    })






}

# Run the application
shinyApp(ui = ui, server = server)
