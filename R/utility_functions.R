


simulate_core <- function(
  input,
  n_executions_game,
  solve_game = FALSE,
  run_parallel = TRUE,
  use_memo = TRUE,
  save_db = FALSE,
  con_db = NULL,
  label = NA,
  i = NA

){



  possible_strategy_dev <- tibble(
    strategy_dev = c(actions$Diligent, actions$Kludgy )
  )

  possible_strategy_rev <- tibble(
    # strategy_rev = c(actions$Careful, actions$Negligent, "NoReview"  )
    strategy_rev = c(actions$Careful, "NoReview"  )
  )

  possible_strategy_meta <- tibble(
    strategy_meta = c(actions$Accurate, actions$Inaccurate, "NoMetaReview"  )
  )

  #retirar
  #no meta review, no review
  possible_devs <- crossing(
    possible_strategy_dev,
    possible_strategy_rev,
    possible_strategy_meta
  ) %>%
    mutate(
      p_dev = row_number()
    )


  unique_games <- expand.grid(1:18, 1:18, 1:18) %>%
    rename(
      dev_1 = 1,
      dev_2 = 2,
      dev_3 = 3
    ) %>%
    mutate(
      id = row_number()
    ) %>%
    pivot_longer(
      cols = -id,
      values_to = "strategy",
      names_to = "dev"
    ) %>%
    group_by(
      id, strategy
    ) %>%
    summarise(
      n = n()
    ) %>%
    pivot_wider(
      names_from = strategy,
      values_from = n,
      values_fill = 0
    ) %>%
    group_by(
      across(
        .cols = matches("[0-9]"),
      )
    ) %>%
    nest() %>%
    ungroup() %>%
    mutate(
      id_unique = row_number()
    ) %>%
    unnest(data) %>%
    select(id, id_unique)


  first_games <- unique_games %>%
    arrange(
      id_unique,
      id
    ) %>%
    group_by(
      id_unique
    ) %>%
    slice_head(
      n = 1
    )


  games_before_prunning <- expand.grid(1:18, 1:18, 1:18) %>%
    rename(
      dev_1 = 1,
      dev_2 = 2,
      dev_3 = 3
    ) %>%
    mutate(
      game = row_number()
    ) %>%
    pivot_longer(
      cols = -game,
      names_to = "dev",
      values_to = "p_dev"
    ) %>%
    left_join(
      possible_devs,
      by = "p_dev"
    ) %>%
    #retirar
    #MEXER
    # filter(
    #   dev == "dev_1" & strategy_dev == "Kludgy" & strategy_rev == "Careful" & strategy_meta == "Accurate" |
    #   dev == "dev_2" & strategy_dev == "Diligent" & strategy_rev == "Negligent" & strategy_meta == "Accurate" |
    #   dev == "dev_3" & strategy_dev == "Diligent" & strategy_rev == "Careful" & strategy_meta == "Inaccurate"
    # ) %>%
    # group_by(
    #   game
    # ) %>%
    # filter(
  #   n() == 3
  # ) %>%
  ungroup()



  info <- list()

  values <- map(
    .x = names(input),
    .f = ~{
      if(.x %in% c("devs_1", "devs_2")){
        info[[.x]] <- hot_to_r(input[[.x]])
      }else{
        info[[.x]] <- input[[.x]]
      }

    }
  )

  names(values) <- names(input)

  values_game <- values[str_detect(string = names(values), pattern = "_game$")]
  names_game <- names(values_game) %>% str_remove(pattern = "_game$")
  names(values_game) <- names_game



  simulate_and_label <- function(
    round,
    values,
    use_memo = TRUE,
    debug = FALSE,
    save_db = FALSE,
    con_db = NULL,
    label = NA,
    i = NA

  ){

    if(use_memo){
      simulated <- simulate_game_simmer_memo(
        values,
        round,
        debug = debug,
        save_db = save_db,
        con_db = con_db,
        label = label,
        i = i

      )
    }else{
      simulated <- simulate_game_simmer(
        values,
        round,
        debug = debug,
        save_db = save_db,
        con_db = con_db,
        label = label,
        i = i
      )
    }

    simulated

  }



  summarise_executions_to_payoff <- function(
    results,
    by_execution = TRUE,
    save_db = FALSE,
    con_db = NULL,
    label = NA,
    i = NA

  ){


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
      mutate(
        pullrequest = str_extract(string = key, pattern = "pr[0-9]*"),
        field = str_extract(string = key, pattern = "(?:_).*") %>% str_remove("_"),
        .before = value
      ) %>%
      select(-key) %>%
      tidyr::pivot_wider(
        names_from = field,
        values_from = value
      ) %>%
      dplyr::arrange(time) %>%
      dplyr::group_by(pullrequest, combination, execution) %>%
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


    # con_db = con_db,
    # save_db = save_db,
    # i = i,
    # label = label,

    if(save_db){

      dbWriteTable(
        conn = con_db,
        name = "simulations",
        value = data %>%
          mutate(
            i = i,
            label = label
          ),
        append = TRUE
      )




    }

    output <- data %>%
      filter(
        cur_stage == "Merge",
        phase == "End"
      ) %>%
      group_by(
        combination,
        execution,
        developer
      ) %>%
      summarise(
        merges = n()
      )


    if(!by_execution){
      output <- output
      group_by(
        developer,
        combination
      ) %>%
        summarise(
          merges = mean(merges)
        )
    }

    output


  }

  simulate_executions <- function(
    values,
    n_executions,
    combination,
    return_results = FALSE,
    use_memo = TRUE,
    debug = FALSE,
    save_db = FALSE,
    con_db = NULL,
    label = NA,
    i = NA



  ){



    result <- map(
      .x = 1:n_executions,
      .f = ~simulate_and_label(
        round = .x,
        values = values,
        use_memo = use_memo,
        save_db = save_db,
        con_db = con_db,
        label = label,
        i = i

      )
    )


    if(return_results){
      if(debug){
        final_result <- map2_df(
          .x = result,
          .y = 1:length(result),
          .f = ~{.x$data %>%
              mutate(
                combination = combination,
                execution = .y
              )}
        )
      }else{
        final_result <- map2_df(
          .x = result,
          .y = 1:length(result),
          .f = ~{.x %>%
              mutate(
                combination = combination,
                execution = .y
              )}
        )
      }

      final_result
    }else{
      tibble(teste = 1)
    }


  }



  create_game <- function(this_game, values){


    saida <- values


    developers <- games %>%
      filter(
        game == this_game
      ) %>%
      dplyr::select(
        strategy_dev,
        strategy_rev,
        strategy_meta
      )

    #mexer
    #browser()
    saida$total_steps <- 550
    # saida$prob_review = 1
    # saida$prob_meta_review = 1
    # saida$entropy_factor =  1.1

    saida$devs <- developers

    saida$combination = this_game

    saida
  }

  game_has_review = values_game$prob_review != 0
  game_has_meta_review = values_game$prob_meta_review != 0

  games <- games_before_prunning %>%
    group_by(game) %>%
    mutate(
      n_no_meta_review = sum(strategy_meta == "NoMetaReview"),
      n_no_review = sum(strategy_rev == "NoReview"),
    ) %>%
    mutate(
      fica_review =
        (game_has_review & n_no_review == 0) |
        (!game_has_review & n_no_review == 3),
      fica_meta_review =
        (game_has_meta_review & n_no_meta_review == 0) |
        (!game_has_meta_review & n_no_meta_review == 3)
    ) %>%
    filter(
      fica_review & fica_meta_review
    ) %>%
    ungroup() %>%
    select(
      -c(
        n_no_meta_review,
        n_no_review,
        fica_review,
        fica_meta_review
      )
    )


  values_game <- map(
    .x = games$game %>% unique() %>% sort(),
    .f = ~create_game(this_game = .x, values = values_game)
  )



  values_game_firsts <- values_game %>%
    enframe() %>%
    mutate(
      game = games$game %>% unique() %>% sort()
    ) %>%
    filter(
      game %in% first_games$id
    )

  values_game_all <- values_game

  values_game <- values_game_firsts$value

  # browser()

  #MEXER
  # values_game <- values_game[sample(x = 1:length(values_game), size = 1)]

  n_game <- n_executions_game

  tictoc::tic("simulate")


  if(run_parallel){

    results_game <- furrr::future_map2_dfr(
      .x = values_game,
      .y = values_game_firsts$game,
      #.y = first_games$id,
      .f = ~simulate_executions(
        values = .x,
        n_executions = n_game,
        combination = .y,
        return_results = solve_game,
        con_db = con_db,
        save_db = save_db,
        i = i,
        label = label,
        use_memo = use_memo  ),
      progress = TRUE,
      .options = furrr::furrr_options(seed = 222 )
    )

    tictoc::toc()


    results_game <- results_game %>%
      summarise_executions_to_payoff(
        con_db = con_db,
        save_db = save_db,
        i = i,
        label = label
      )


  }else{


    tictoc::tic("solve")

    results_game <- purrr::map2_dfr(
      .x = values_game,
      .y = 1:length(values_game),
      .f = ~simulate_executions(
        values = .x,
        n_executions = n_game,
        combination = .y,
        return_results = solve_game ,
        use_memo = use_memo,
        con_db = con_db,
        save_db = save_db,
        i = i,
        label = label,
      )
    ) %>%
      summarise_executions_to_payoff(
        con_db = con_db,
        save_db = save_db,
        i = i,
        label = label,

      )

    tictoc::toc()




  }




  games_for_results_final <- games %>%
    group_by(
      game,
      strategy_dev,
      strategy_meta,
      strategy_rev
    ) %>%
    mutate(
      n_strategy = row_number()
    ) %>%
    ungroup() %>%
    mutate(
      dev = str_extract(dev, pattern = "[0-9]+") %>%  as.integer()
    )

  results_run <- results_game %>%
    inner_join(
      games_for_results_final,
      by = c("developer" = "dev", "combination" = "game")
    )



  results_game_final <- first_games %>%
    rename(id_run = id) %>%
    inner_join(
      unique_games %>%  rename(id_receive = id),
      by = c("id_unique")
    ) %>%
    inner_join(
      games_for_results_final,
      by = c("id_receive" = "game")
    ) %>%
    inner_join(
      results_run,
      by = c(
        "id_run" = "combination",
        "strategy_dev",
        "strategy_rev",
        "strategy_meta",
        "n_strategy"
      )
    ) %>%
    ungroup() %>%
    select(
      combination = id_receive,
      execution,
      developer = dev,
      merges
    )



  format_games_log <- function(values, i){
    values %>% enframe() %>%  pivot_wider(names_from = "name", values_from = "value" ) %>%
      unnest() %>%
      mutate(
        dev = 1:3
      )
  }


  value_games_formatado <- map2_dfr(
    .x = values_game_all,
    .y = 1:length(values_game_all),
    .f = format_games_log
  )

  #solve


  if(solve_game){

    eqs_game <- prepare_and_solve_game(params = values_game_all, results = results_game_final)

    saida <- eqs_game

  }else{
    saida = 0
  }



  if(save_db){


    if(!is.null(saida) & length(saida) != 1){
      dbWriteTable(
        conn = con_db,
        name = "eqs",
        value = saida %>%
          mutate(
            i = i,
            label = label
          ),

        append = TRUE
      )

    }


    results_with_params <- value_games_formatado %>%
      inner_join(
        results_game_final,
        by = c("combination", "dev" = "developer")
      )

    dbWriteTable(
      conn = con_db,
      name = "results",
      value = results_with_params %>%
        mutate(
          i = i,
          label = label
        )
      ,
      append = TRUE
    )

    dbWriteTable(
      conn = con_db,
      name = "params",
      value = value_games_formatado %>%
        mutate(
          i = i,
          label = label
        ),
      append = TRUE
    )


    dbWriteTable(
      conn = con_db,
      name = "labels",
      value = tibble(
        label = label,
        name = input$name
      ),
      append = TRUE
    )



  }



  saida


}




simulate_game_simmer_memo <- memoise(simulate_game_simmer, cache = local_cache  )

simulate_core_memo <- memoise(simulate_core, cache = local_cache)




















