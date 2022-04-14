#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#test

library(shiny)
library(tidyverse)
library(ggiraph)
library(reactable)
library(DBI)

con_db <- dbConnect(RSQLite::SQLite(),"db/log.db" )


instances <- read_rds("data2/instances")


files <- list.files(path = "data2", pattern = "saida_final", full.names = TRUE) %>%
    enframe() %>%
    rowwise() %>%
    mutate(
        conteudo = list(read_rds(value))
    ) %>%
    mutate(
        instance = str_extract(value, "[0-9]*$") %>%  as.integer()
    ) %>%
    rowwise() %>%
    mutate(
        results = list(conteudo$resultados),
        eqs = list(conteudo$eqs),

    )


eqs_old <- files %>%
    select(
        instance,
        eqs
    ) %>%
    unnest(eqs) %>%
    left_join(
        instances,
        by = "instance"
    )


params_db  <- tbl(con_db, "params") %>%  collect()

params_label_i <- params_db %>% select(
    -c( dev, strategy_dev, strategy_rev, strategy_meta, combination)
) %>%
    distinct()


eqs <- tbl(con_db, "eqs") %>%  collect() %>%
    inner_join(
        params_label_i,
        by = c("label", "i")
    ) %>%
    separate(
        col = complete_strategy_1,
        into = c("strategy_dev_1", "strategy_rev_1", "strategy_meta_1")
    ) %>%
    separate(
        col = complete_strategy_2,
        into = c("strategy_dev_2", "strategy_rev_2", "strategy_meta_2")
    ) %>%
    separate(
        col = complete_strategy_3,
        into = c("strategy_dev_3", "strategy_rev_3", "strategy_meta_3")
    ) %>%
    mutate(
        across(
            starts_with("strategy_rev"),
            .fns = ~if_else(prob_review == 0, "NoReview", .x )
        )
    ) %>%
    mutate(
        across(
            starts_with("strategy_meta"),
            .fns = ~if_else(prob_review == 0 | prob_meta_review == 0, "NoMetaReview", .x )
        )
    ) %>%
    select(-eq) %>%
    distinct() %>%
    mutate(
        instance = label * 1000 + i
    ) %>%
    group_by(
        instance
    ) %>%
    mutate(
        eq = row_number()
    ) %>%
    ungroup() %>%
    unite(
        col = "complete_strategy_1",
        strategy_dev_1,
        strategy_rev_1,
        strategy_meta_1,
        sep = "_"
    ) %>%
    unite(
        col = "complete_strategy_2",
        strategy_dev_2,
        strategy_rev_2,
        strategy_meta_2,
        sep = "_"
    ) %>%
    unite(
        col = "complete_strategy_3",
        strategy_dev_3,
        strategy_rev_3,
        strategy_meta_3,
        sep = "_"
    ) %>%
    mutate(
        process_fraction_review_game = prob_review * 100
    ) %>%
    mutate(
        process_fraction_metareview_game = prob_meta_review * 100
    ) %>%
    mutate(
        characteristics_kludges_harmful_game = case_when(
            entropy_factor == 1 ~ "No",
            entropy_factor == 1.05 ~ "Moderately",
            entropy_factor == 1.1 ~ "A great deal",
        )
    ) %>%
    mutate(
        characteristics_kludges_harmful_game = fct_reorder(.f = characteristics_kludges_harmful_game, .x = entropy_factor )
    ) %>%
    mutate(
        order = entropy_factor
    )



results_old <- files %>%
    select(
        instance,
        results
    ) %>%
    unnest(results) %>%
    left_join(
        instances,
        by = "instance"
    )



results <- tbl(con_db, "results") %>%
    collect() %>%
    mutate(
        instance = label * 1000 + i
    )



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Solved Games"),

    # Sidebar with a slider input for number of bins
    fluidPage(
        # Show a plot of the generated distribution
        verticalLayout(

            radioButtons(
                inputId = "label",
                label = "Case",
                choiceNames = labels$name,
                choiceValues = labels$label
            ),

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


    games <- reactive(
        eqs %>%
            filter(
                label == input$label
            ) %>%
            group_by(
                process_fraction_review_game,
                process_fraction_metareview_game,
                characteristics_kludges_harmful_game,
                order,
                instance,
                i
            ) %>%
            summarise(
                n = n()
            ) %>%
            mutate(
                neg_order = -1*order,
                characteristics_kludges_harmful_game = fct_reorder(.f = characteristics_kludges_harmful_game, .x = neg_order, .desc = TRUE )
            )
    )



    output$main <- renderGirafe({


        grafico <- ggplot(games()) +
            geom_tile_interactive(
                aes(
                    y = process_fraction_review_game,
                    x = process_fraction_metareview_game,
                    data_id = i
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
                breaks = c(0, 50, 100),
                minor_breaks = c(0, 50, 100),
                sec.axis = dup_axis()
            ) +
            scale_y_continuous(
                breaks = c(0, 50, 100),
                minor_breaks = c(0, 50, 100),
                sec.axis = dup_axis()
            ) +
            labs(
                y = "% Review",
                x = "% Metareview"
            )

        girafe(ggobj = grafico, width_svg = 20, height_svg = 5, options = list(opts_selection(selected = "1", type = "single")))


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



    results_selected <- reactive({




    })


    output$results <- renderReactable({


        eqs_selected <- eqs %>%
            filter(
                label == input$label,
                i == input$main_selected
            ) %>%
            select(
                starts_with("complete_strategy")
            )


        reactable(
            eqs_selected,
            defaultPageSize = 600
        )

    })

}

# Run the application
shinyApp(ui = ui, server = server)
