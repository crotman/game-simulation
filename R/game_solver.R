


prepare_and_solve_game <- function(params, results){

  results_grouped <- results %>%
    group_by(
      combination,
      developer
    ) %>%
    summarise(
      merges = mean(merges)
    )

  strategies <- prepare_strategies_to_solve(params)

  output <- solve_game_gambit(strategies, results_grouped)

  # browser()

  output

}


prepare_strategies_to_solve <-  function(params){


  strategies <- purrr::map2_df(
    .x = params,
    .y = 1:length(params),
    .f = ~{
      .x$devs %>%
        dplyr::mutate(
          dev = row_number(),
          combination = .x$combination
        )
    }
  ) %>%
    tidyr::unite(
      col = "complete_strategy",
      strategy_dev,
      strategy_rev,
      strategy_meta,
      remove = FALSE
    )


  strategies

}


solve_game <- function(strategies, results){

  devs <- strategies$dev %>% unique()

  strategy_stages <- purrr::map(
    .x = devs,
    .f = ~{

      actions <- strategies %>%
        dplyr::filter(
          dev == .x
        ) %>%
        dplyr::pull(
          complete_strategy
        ) %>%
        unique()

      saida <- stage(
        name = "strategy_stage" ,
        player = .x,
        actions = list(
          action(stringr::str_glue("complete_strategy_{.x}"), actions )
        )
      )
    }
  )

  strategy_declaration <- paste0(

    "strategies <- tibble::tibble(\n",

    "complete_strategy = c(\"", str_flatten(strategies$complete_strategy, collapse = "\", \""), "\"),\n",
    "strategy_dev = c(\"", str_flatten(strategies$strategy_dev, collapse = "\", \""), "\"),\n",
    "strategy_rev = c(\"", str_flatten(strategies$strategy_rev, collapse = "\", \""), "\"),\n",
    "strategy_meta = c(\"", str_flatten(strategies$strategy_meta, collapse = "\", \""), "\"),\n",
    "combination = c(", str_flatten(strategies$combination, collapse = "L, "), "L),\n",
    "dev = c(", str_flatten(strategies$dev, collapse = "L, "), "L)\n",


    ")\n"
  )


  results_declaration <- paste0(

    "results <- tibble::tibble(\n",

    "developer = c(", str_flatten(results$developer, collapse = "L, "), "),\n",
    "combination = c(", str_flatten(results$combination, collapse = "L, "), "L),\n",
    "merges = c(", str_flatten(results$merges, collapse = ", "), ")\n",


    ")\n"
  )



  calculate_payoff <<- function(player,  ...){


    dot_args <- list(...)


    calculate_payoff_inside <-  function(params, strategies, results){


      cur_params <- params %>%
        dplyr::mutate(
          dev = stringr::str_extract(name, pattern = "[0-9]*$") %>%  as.integer()
        )


      current_combination <- strategies  %>%
        dplyr::inner_join(
          cur_params,
          by = c("dev", "complete_strategy" = "value")
        ) %>%
        dplyr::group_by(
          combination
        ) %>%
        dplyr::filter(
          dplyr::n() == 3
        ) %>%
        dplyr::pull(combination) %>%
        dplyr::first()


      if(length(current_combination) > 0){


        payoff <- results %>%
          dplyr::filter(
            combination == current_combination,
            developer == player
          ) %>%
          dplyr::pull(merges) %>%
          first()

        output <- payoff

      }else{
        output <- 0
      }

      if(length(output != 0)){
        output
      }else
          0
      }



    output <- dot_args %>%
      tibble::enframe() %>%
      tidyr::unnest(value) %>%
      dplyr::group_by(
        name
      ) %>%
      dplyr::mutate(
        instance = dplyr::row_number()
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(
        instance
      ) %>%
      tidyr::nest() %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        payoff = calculate_payoff_inside(params = data, strategies = strategies, results = results)
      ) %>%
      tidyr::replace_na(
        list(payoff = 0)
      ) %>%
      pull(payoff)


    output


  }


  params_devs <- str_glue("complete_strategy_{1:length(devs)} = complete_strategy_{1:length(devs)}") %>%
    str_flatten(
      collapse = ", "
    )


  payoff_stages <- list(stage("payoff_stage",
                       player=1:length(devs),

                       compute = purrr::map(
                         .x = 1:length(devs),
                         .f = ~as.formula("payoff_{.x} ~ calculate_payoff(player = {.x}, {params_devs} )" %>%  str_glue() %>%  as.character() ),
                       )

                    ))


  game_stages <- c(strategy_stages, payoff_stages)


  # browser()


  game = new_game(
    gameId = "Simulation",
    options = make_game_options(verbose=TRUE),
    params = list(numPlayers=length(devs)),
    stages = game_stages
  )


    eq <- game %>%
    game_solve()




    tryCatch(
      {

        eq_table_game <- eq %>%
          eq_tables(
            reduce.tables = FALSE,
            combine = 0
          )

        saida  <- eq_table_game %>%
          map2_df(
            .y = 1:length(.),
            .f = function(.x, .y){
              tibble(
                eq = .y,
                strategy_1 <- .x$complete_strategy_1,
                strategy_2 <- .x$complete_strategy_2,
                strategy_3 <- .x$complete_strategy_3
              )
            }
          )
      },

      error = saida <- NULL

  )



  saida_resultados <- get_outcomes(game)

  list(
    eqs = saida,
    resultados = saida_resultados
  )

}


solve_game_gambit <- function(strategies, results){


  n_strategies <- strategies$complete_strategy %>% unique() %>%  length()

  n_developers <- strategies$dev %>%  unique() %>% length()


  gambit_players <- 1:n_developers %>%
    enframe(
      name = "player"
    ) %>%
    select(player)

  gambit_strategies <- strategies$complete_strategy %>% unique() %>%  sort() %>%
    enframe(
      name = "id_strategy",
      value = "strategy"
    )


  players_content <- str_glue("\"{gambit_players$player}\"") %>% str_flatten(collapse = " " )

  gambit_content_header <- paste0("NFG 1 R \"Prisoner\" {", players_content, "} ")

  strategies_content <- paste0(
    "{",
    str_glue("\"{gambit_strategies$strategy}\"") %>% str_flatten(collapse = " " ),
    "}"
  )

  gambit_content_strategies <- reduce(
    replicate(n = nrow(gambit_players) , expr = strategies_content, simplify = FALSE ),
    .f = ~paste0(.x, "\n", .y )
  )

  gambit_content_strategies <- paste0("{\n", gambit_content_strategies, "\n}" )

  strategy_fields <- str_glue("id_strategy_{n_developers:1}")

  merge_fields <- paste0(
    "{",
    str_flatten(str_glue("merges_{1:nrow(gambit_players)}"),collapse = "}, {"),
    "}"
  )


  payoff_content_df <- results %>%
    inner_join(
      strategies,
      by = c("developer" = "dev", "combination")
    ) %>%
    inner_join(
      gambit_strategies,
      by = c("complete_strategy" = "strategy")
    ) %>%
    select(
      developer, merges, id_strategy, combination
    ) %>%
    pivot_wider(
      values_from = c(merges, id_strategy),
      names_from = developer
    ) %>%
    arrange(
      across(
        .cols = c(strategy_fields )
      )
    ) %>%
    mutate(
      payoff = str_glue(merge_fields)
    ) %>%
    mutate(
      payoff_content = paste0(
        "{ ",
        "\"\" ",
        payoff,
        "}"
      )
    )

  # browser()

  payoff_content <- paste0(
    "{\n",
    str_flatten(payoff_content_df$payoff_content, collapse = "\n"),
    "\n}"
  )

  last_line <- 1:nrow(payoff_content_df) %>%
    str_flatten(
      collapse = " "
    )

  nfg_content <- paste0(
    gambit_content_header,
    "\n",
    "\n",
    gambit_content_strategies,
    "\n",
    "\n",
    payoff_content,
    "\n",
    last_line
  )

  new_uuid <- uuid::UUIDgenerate()

  write_file(
    nfg_content,
    new_uuid
  )

  command <- paste(
    sep = " ",
    here("gambit/gambit-enumpure.exe"),
    here(new_uuid)
  )


  output <- system(command =  command, intern = TRUE) %>%
    last() %>%
    str_split(
      pattern = ","
    ) %>%
    enframe() %>%
    unnest(value) %>%
    filter(
      value != "NE"
    ) %>%
    mutate(
      index = row_number() - 1
    ) %>%
    mutate(
      player = index %/% n_strategies + 1,
      strategy = index %% n_strategies + 1,
      eq = index %/% (n_developers * n_strategies) + 1
    ) %>%
    filter(
      value == 1
    ) %>%
    select(
      eq, player, strategy
    ) %>%
    left_join(
      gambit_strategies %>%  rename(complete_strategy = strategy),
      by = c("strategy" = "id_strategy")
    ) %>%
    select(-strategy) %>%
    pivot_wider(
      names_from = player,
      values_from = complete_strategy,
      names_prefix = "complete_strategy_"
    )






}






