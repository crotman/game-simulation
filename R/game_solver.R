



solve_game <- function(params, results){

  params <- read_rds("values.rds")

  results <- read_rds("results.rds")

  strategies <- map2_df(
    .x = params,
    .y = 1:length(params),
    .f = ~{
      .x$devs %>%
        mutate(
          combination = .y,
          dev = row_number()
        )
    }
  ) %>%
    unite(
      col = "complete_strategy",
      strategy_dev,
      strategy_rev,
      strategy_meta,
      remove = FALSE
    )



  devs <- strategies$dev %>% unique()

  strategy_stages <- map(
    .x = devs,
    .f = ~{

      actions <- strategies %>%
        filter(
          dev == .x
        ) %>%
        pull(
          complete_strategy
        ) %>%
        unique()

      saida <- stage(
        name = "strategy_stage" ,
        player = .x,
        actions = list(
          action(str_glue("complete_strategy_{.x}"), actions )
        )
      )
    }
  )

  calculate_payoff <- function(player,  ...){

    dot_args <- list(...)

    tibble_dot_args <- dot_args %>%
      enframe() %>%
      unnest(value) %>%
      mutate(
        dev = str_extract(name, pattern = "[0-9]*$") %>%  as.integer()
      )

    current_combination <- strategies %>%
      inner_join(
        tibble_dot_args,
        by = c("dev", "complete_strategy" = "value")
      ) %>%
      group_by(
        combination
      ) %>%
      filter(
        n() == 3
      ) %>%
      pull(combination) %>%
      first()



    if(length(current_combination) > 0){

      payoff <- results %>%
        filter(
          FALSE,
          combination == current_combination,
          developer == player
        )  %>%
        pull(merges) %>%
        first()

      output <- payoff
    }else{
      output <- 0
    }



    output
  }


  params_devs <- str_glue("complete_strategy_{1:length(devs)} = complete_strategy_{1:length(devs)}") %>%
    str_flatten(
      collapse = ", "
    )

  payoff_stages <- list(stage("payoff_stage",
                       player=1:length(devs),

                       compute = map(
                         .x = 1:length(devs),
                         .f = ~as.formula("payoff_{.x} ~ calculate_payoff(player = {.x}, {params_devs} )" %>%  str_glue()),
                       )

                       # compute=list(
                       #   as.formula("payoff_1 ~ calculate_payoff(player = 1, testimonial_1 = testimonial_1, testimonial_2 = testimonial_2)"),
                       #   as.formula("payoff_2 ~ calculate_payoff(player = 2, testimonial_1 = testimonial_1, testimonial_2 = testimonial_2)")
                       # )
                    ))


  game_stages <- c(strategy_stages, payoff_stages)

  game = new_game(
    gameId = "Simulation",
    options = make_game_options(verbose=TRUE),
    params = list(numPlayers=length(devs)),
    stages = game_stages
  )

  eq <- game %>%
    game_solve() %>%
    eq_tables(combine = 2) %>%
    print()


}



params[[1]]$devs %>%
  mutate(
    combination = .y,
    dev = row_number()
  )
