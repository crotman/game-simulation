#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggiraph)
library(reactable)

eqs <- read_rds("data/eqs.rds")
results <- read_rds("data/results.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Solved Games"),

    # Sidebar with a slider input for number of bins
    fluidPage(
        # Show a plot of the generated distribution
        verticalLayout(
            girafeOutput(
                outputId = "main"
            ),
            checkboxInput(inputId = "only_eq", label = "Only Equilibria?", value = FALSE),
            reactableOutput(outputId = "results")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


    output$main <- renderGirafe({

        games <- eqs %>%
            group_by(
                process_fraction_review_game,
                process_fraction_metareview_game,
                characteristics_kludges_harmful_game,
                order
            ) %>%
            summarise(
                n = n(),
                instance = mean(instance)
            ) %>%
            mutate(
                neg_order = -1*order,
                characteristics_kludges_harmful_game = fct_reorder(.f = characteristics_kludges_harmful_game, .x = neg_order, .desc = TRUE )
            )

        grafico <- ggplot(games) +
            geom_tile_interactive(
                aes(
                    y = process_fraction_review_game,
                    x = process_fraction_metareview_game,
                    data_id = instance
                ) ,
                color = "white",
                fill = "darkgreen",
                size = 2
            ) +
            geom_text(
                aes(
                    y = process_fraction_review_game,
                    x = process_fraction_metareview_game,
                    label =  n
                ) ,
                color = "white"
            ) +
            facet_wrap(
                ~characteristics_kludges_harmful_game,
                nrow = 1,
                as.table = FALSE
            ) +
            theme_minimal() +
            scale_x_continuous(
                breaks = c(0, 25, 50, 75, 100),
                minor_breaks = c(0, 25, 50, 75, 100),
                sec.axis = dup_axis()
            ) +
            scale_y_continuous(
                breaks = c(0, 25, 50, 75, 100),
                minor_breaks = c(0, 25, 50, 75, 100),
                sec.axis = dup_axis()
            ) +
            labs(
                y = "% Review",
                x = "% Metareview"
            )

        girafe(ggobj = grafico, width_svg = 20, height_svg = 5, options = list(opts_selection(selected = "50", type = "single")))


    })


    output$eqs <- renderReactable({


        eqs_selected <- eqs %>%
            filter(
                instance == input$main_selected
            ) %>%
            select(
                starts_with("complete_")
            )

        reactable(
            eqs_selected
        )

    })


    output$results <- renderReactable({


        print(input$main_selected)

        eqs_join <- eqs %>%
            filter(
                instance == input$main_selected
            ) %>%
            select(starts_with("complete_"), instance) %>%
            mutate(
                eq = "YES"
            )


        results_selected <- results %>%
            filter(
                instance == input$main_selected
            ) %>%
            select(
                starts_with("complete_") | starts_with("payoff_")
            ) %>%
            mutate(
                total_payoff = payoff_1 + payoff_2 + payoff_3
            ) %>%
            left_join(
                eqs_join,
                by = c("complete_strategy_1", "complete_strategy_2", "complete_strategy_3")
            ) %>%
            select(-instance) %>%
            filter(
                eq == "YES" | !input$only_eq
            ) %>%
            separate(
                col = "complete_strategy_1",
                into = c("Dev_1", "Review_1", "Meta_1"),
                sep = "_"
            ) %>%
            separate(
                col = "complete_strategy_2",
                into = c("Dev_2", "Review_2", "Meta_2"),
                sep = "_"
            ) %>%
            separate(
                col = "complete_strategy_3",
                into = c("Dev_3", "Review_3", "Meta_3"),
                sep = "_"
            )



        reactable(
            results_selected,
            defaultPageSize = 600
        )

    })

}

# Run the application
shinyApp(ui = ui, server = server)
