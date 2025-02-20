library(tidyverse)
library(memoise)

likert_harm <- tibble(
  entropy = c(1, 1.0025, 1.005, 1.0075, 1.01, 1.0125, 1.015, 1.0175, 1.02),
  description = c("1", "1.0025", "1.005", "1.0075", "1.01", "1.0125", "1.015", "1.0175", "1.02")
)


local_cache <- cachem::cache_disk("data", max_size = Inf )


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






update_input <- function(input, process_fraction_review_game, process_fraction_metareview_game, characteristics_kludges_harmful_game ){


  input$prob_review_game = process_fraction_review_game/100
  input$prob_meta_review_game = process_fraction_metareview_game/100

  harm_degree_game <- characteristics_kludges_harmful_game

  if(!is.null(harm_degree_game)){

    entropy_number_game <- likert_harm %>%
      filter(
        description == harm_degree_game
      ) %>%
      pull(entropy)

    input$entropy_factor_game = entropy_number_game

  }

  input

}



simulation_results <- eventReactive(eventExpr = input$solve , valueExpr = {

  simulate_core(input = input, n_executions_game = n_executions_game() )

})


simulate_all <- eventReactive(eventExpr = input$solve_all , valueExpr = {


  tib_process_fraction_review_game <-  tibble(
    process_fraction_review_game = c(0, 50, 100)
  )

  tib_process_fraction_metareview_game <-  tibble(
    process_fraction_metareview_game = c(0, 50, 100)
  )

  tib_characteristics_kludges_harmful_game <- tibble(
    characteristics_kludges_harmful_game = likert_harm$description,
    order = likert_harm$entropy
  )



  all_params_all <- crossing(
    tib_process_fraction_review_game,
    tib_process_fraction_metareview_game,
    tib_characteristics_kludges_harmful_game
  ) %>%
    arrange(
      process_fraction_review_game,
      process_fraction_metareview_game,
      order
    ) %>%
    mutate(
      instance = row_number()
    )


  files <- list.files(pattern = "[0-9]+.rds") %>%
    str_remove("\\.rds") %>%
    as.integer() %>%
    enframe() %>%
    rename(
      instance = value
    )


  all_params <- all_params_all %>%
    anti_join(files, by = "instance") %>%
    sample_n(size = nrow(.))


  for (i in 1:nrow(all_params)){


    info <- list()

    values <- map(
      .x = names(input),
      .f = ~{
        info[[.x]] <- input[[.x]]
      }
    )

    names(values) <- names(input)

    this_input <- update_input(
      input = values,
      process_fraction_review_game = all_params$process_fraction_review_game[i],
      process_fraction_metareview_game = all_params$process_fraction_metareview_game[i],
      characteristics_kludges_harmful_game = all_params$characteristics_kludges_harmful_game[i]
    )



    simulate_core_memo(input = this_input, n_executions_game = n_executions_game() )

    write_rds(all_params$instance[i], paste0( all_params$instance[i], ".rds"))


  }


})



read_all <- eventReactive(eventExpr = input$read_solve_all , valueExpr = {



  tib_process_fraction_review_game <-  tibble(
    process_fraction_review_game = c(0, 25, 50, 75, 100)
  )

  tib_process_fraction_metareview_game <-  tibble(
    process_fraction_metareview_game = c(0, 25, 50, 75, 100)
  )

  tib_characteristics_kludges_harmful_game <- tibble(
    characteristics_kludges_harmful_game = likert_harm$description,
    order = likert_harm$entropy
  )

  all_params_all <- crossing(
    tib_process_fraction_review_game,
    tib_process_fraction_metareview_game,
    tib_characteristics_kludges_harmful_game
  ) %>%
    arrange(
      process_fraction_review_game,
      process_fraction_metareview_game,
      order
    ) %>%
    mutate(
      instance = row_number()
    )




  files <- list.files(pattern = "[0-9]+.rds") %>%
    str_remove("\\.rds") %>%
    as.integer() %>%
    enframe() %>%
    rename(
      instance = value
    )


  all_params <- all_params_all %>%
    semi_join(files, by = "instance") %>%
    sample_n(size = nrow(.))


  saida <- list()

  for (i in 1:nrow(all_params)){


    info <- list()

    values <- map(
      .x = names(input),
      .f = ~{
        info[[.x]] <- input[[.x]]
      }
    )

    names(values) <- names(input)

    this_input <- update_input(
      input = values,
      process_fraction_review_game = all_params$process_fraction_review_game[i],
      process_fraction_metareview_game = all_params$process_fraction_metareview_game[i],
      characteristics_kludges_harmful_game = all_params$characteristics_kludges_harmful_game[i]
    )

    print(i)
    print(all_params$instance[i])

    elemento <- simulate_core_memo(input = this_input, n_executions_game = n_executions_game(), solve_game = TRUE  )

    saida[[i]] <- elemento

    saida[[i]]$instance <-  all_params$instance[i]

  }


  eqs <- map_df(
    .x = saida,
    .f = ~{
      .x$eqs %>%
        mutate(
          instance = .x$instance
        )
    }
  ) %>%
    inner_join(
      all_params,
      by = "instance"
    )


  results <- map_df(
    .x = saida,
    .f = ~{
      .x$resultados %>%
        mutate(
          instance = .x$instance
        )
    }
  ) %>%
    inner_join(
      all_params,
      by = "instance"
    )

  write_rds(eqs, "solved-games/data/eqs.rds")
  write_rds(results, "solved-games/data/results.rds")


  saida

})


create_inputs <- function(suffix){

  wellPanel(

    h3("Case {suffix}" %>%  str_glue()),
    tags$hr(),
    selectInput(
      inputId = "dev_1_{suffix}" %>%  str_glue(),
      label = "Strategy dev1",
      choices = c(actions$Kludgy, actions$Diligent)
    ),
    selectInput(
      inputId = "dev_2_{suffix}" %>%  str_glue(),
      label = "Strategy dev2",
      choices = c(actions$Kludgy, actions$Diligent)
    ),
    selectInput(
      inputId = "dev_3_{suffix}" %>%  str_glue(),
      label = "Strategy dev3",
      choices = c(actions$Kludgy, actions$Diligent)
    ),

    sliderInput("process_fraction_review_{suffix}" %>%  str_glue(),label="% of pull requests reviewed", min = 0, max = 100, post  = " %", value = 50, step = 5),
    sliderInput("entropy_factor_{suffix}"  %>%  str_glue() ,label="% degradation for each kludge", min = 0, max = 5, post  = " %", value = 1, step = 0.25),
    sliderInput("process_fraction_metareview_{suffix}" %>%  str_glue(),label="% of reviews metareviewed", min = 0, max = 100, post  = " %", value = 50, step = 5) %>%  hidden(),


    #### Game ####


    numericInput(
      inputId = "total_steps_{suffix}" %>% str_glue(),
      label = "$$Steps$$" %>% withMathJax(),
      value = "2000",
      step = 1
    ) %>%  hidden(),

    # numericInput(
    #   inputId = "entropy_factor_{suffix}" %>% str_glue(),
    #   label = "$$H$$" %>% withMathJax(),
    #   value = "1.02",
    #   step = 0.01
    # ) %>%  hidden() ,

    numericInput(
      inputId = "prob_review_{suffix}" %>% str_glue(),
      label = "$$P(Review)$$" %>% withMathJax(),
      value = "0.5"
    ) %>%  hidden() ,

    numericInput(
      inputId = "prob_meta_review_{suffix}" %>% str_glue(),
      label = "$$P(MetaReview|Review)$$"  %>% withMathJax(),
      value = "0.5"
    ) %>%  hidden() ,

    numericInput(
      inputId = "lambda_{suffix}" %>% str_glue(),
      label = "$$\\lambda$$"  %>% withMathJax(),
      value = "1"
    ) %>%  hidden() ,

    numericInput(
      inputId = "n_executions_{suffix}" %>% str_glue(),
      label = "$$executions$$"  %>% withMathJax(),
      value = "5"
    ) %>%  hidden(),


    #### Development Stage #####



    numericInput(
      inputId = "prob_diligent_{suffix}" %>% str_glue(),
      label = "$$P(K)|D$$" %>%  withMathJax(),
      value = "0.1",
      step = 0.01
    ) %>%  hidden(),


    numericInput(
      inputId = "prob_kludgy_{suffix}" %>% str_glue(),
      label = "$$P(K)|K$$" %>%  withMathJax(),
      value = "0.9",
      step = 0.01
    ) %>%  hidden(),

    numericInput(
      inputId = "mean_time_diligent_{suffix}" %>% str_glue(),
      label = withMathJax("$$\\mu|(D, !R)$$"),
      value = "10",
      step = "1"
    ) %>%  hidden(),

    numericInput(
      inputId = "mean_time_kludgy_{suffix}" %>% str_glue(),
      label = withMathJax("$$\\mu|(K, !R)$$"),
      value = "6",
      step = "1"
    ) %>%  hidden(),

    numericInput(
      inputId = "sd_time_develop_{suffix}" %>% str_glue(),
      label = withMathJax("$$\\sigma|(!R)$$"),
      value = "2",
      step = "1"
    ) %>%  hidden(),

    numericInput(
      inputId = "mean_time_rework_{suffix}" %>% str_glue(),
      label = withMathJax("$$\\mu|R$$"),
      value = "3",
      step = "1"
    ) %>%  hidden(),

    numericInput(
      inputId = "sd_time_rework_{suffix}" %>% str_glue(),
      label = withMathJax("$$\\sigma|R$$"),
      value = "5",
      step = "1"
    ) %>%  hidden(),


    ####  Review Stage #####


    numericInput(
      inputId = "mean_time_review_{suffix}" %>% str_glue(),
      label = "$$\\mu$$",
      value = "1",
      step = "1"
    ) %>%  hidden(),

    numericInput(
      inputId = "sd_time_review_{suffix}" %>% str_glue(),
      label = "$$\\sigma$$",
      value = "0.2",
      step = "1"
    ) %>%  hidden(),


    numericInput(
      inputId = "prob_kludgy_review_when_kludge_careful_{suffix}" %>% str_glue(),
      label = "$$P(K)|(K, C)$$" %>% withMathJax() ,
      value = "0.85"
    ) %>%  hidden(),

    numericInput(
      inputId = "prob_kludgy_review_when_not_kludge_careful_{suffix}" %>% str_glue(),
      label = "$$P(K)|(!K, C)$$" %>% withMathJax(),
      value = "0.1"
    ) %>%  hidden(),

    numericInput(
      inputId = "prob_kludgy_review_when_kludge_negligent_{suffix}" %>% str_glue(),
      label = "$$P(K)|(K, N)$$" %>% withMathJax(),
      value = "0.25"
    ) %>%  hidden(),

    numericInput(
      inputId = "prob_kludgy_review_when_not_kludge_negligent_{suffix}" %>% str_glue(),
      label = "$$P(K)|(!K, N)$$" %>% withMathJax(),
      value = "0.1"
    ) %>%  hidden(),


    #### metarevivew #####

    numericInput(
      inputId = "prob_negative_when_correct_kludgy_accurate_{suffix}" %>% str_glue(),
      label = "$$P(-)|(K, +, A)$$" %>%  withMathJax(),
      value = "0.1",
      step = 0.01
    ) %>%  hidden(),

    numericInput(
      inputId = "prob_negative_when_correct_not_kludgy_accurate_{suffix}" %>% str_glue(),
      label = "$$P(-)|(!K, +, A)$$"  %>%  withMathJax(),
      value = "0.1",
      step = 0.01
    ) %>%  hidden(),

    numericInput(
      inputId = "prob_negative_when_incorrect_kludgy_accurate_{suffix}" %>% str_glue(),
      label = "$$P(-)|(K, -, A)$$"  %>%  withMathJax(),
      value = "0.85",
      step = 0.01
    ) %>%  hidden(),

    numericInput(
      inputId = "prob_negative_when_incorrect_not_kludgy_accurate_{suffix}" %>% str_glue(),
      label = "$$P(-)|(!K, -, A)$$"  %>%  withMathJax(),
      value = "0.85",
      step = 0.01
    ) %>%  hidden(),


    numericInput(
      inputId = "prob_negative_when_correct_kludgy_inaccurate_{suffix}" %>% str_glue(),
      label = "$$P(-)|(K, +, I)$$"  %>%  withMathJax(),
      value = "0.6",
      step = 0.01
    ) %>%  hidden(),

    numericInput(
      inputId = "prob_negative_when_correct_not_kludgy_inaccurate_{suffix}" %>% str_glue(),
      label = "$$P(-)|(!K, +, I)$$"  %>%  withMathJax(),
      value = "0.1",
      step = 0.01
    ) %>%  hidden(),

    numericInput(
      inputId = "prob_negative_when_incorrect_kludgy_inaccurate_{suffix}" %>% str_glue(),
      label = "$$P(-)|(K, -, I)$$"  %>%  withMathJax(),
      value = "0.1",
      step = 0.01
    ) %>%  hidden(),

    numericInput(
      inputId = "prob_negative_when_incorrect_not_kludgy_inaccurate_{suffix}" %>% str_glue(),
      label = "$$P(-)|(!K, -, I)$$"  %>%  withMathJax(),
      value = "0.9",
      step = 0.01
    ) %>%  hidden(),

    numericInput(
      inputId = "mean_time_accurate_{suffix}" %>% str_glue(),
      label = "$$\\mu|A$$"  %>%  withMathJax() ,
      value = "1",
      step = "1"
    ) %>%  hidden(),

    numericInput(
      inputId = "mean_time_inaccurate_{suffix}" %>% str_glue(),
      label = "$$\\mu|I$$"  %>%  withMathJax(),
      value = "1",
      step = "1"
    ) %>%  hidden(),


    numericInput(
      inputId = "sd_time_meta_review_{suffix}" %>% str_glue(),
      label = "$$\\sigma$$"  %>%  withMathJax(),
      value = "0.2",
      step = "0.1"
    ) %>%  hidden()




  )



}



simulate_action_button <- function(input, progress){

  n_executions <- input$n_executions

  info <- list()

  values <- map(
    .x = names(input),
    .f = ~{
      info[[.x]] <- input[[.x]]
    }
  )


  devs_1 <- tribble(

    ~strategy_dev, ~strategy_rev, ~strategy_meta,
    input$dev_1_1, actions$Careful, actions$Accurate,
    input$dev_2_1, actions$Careful, actions$Accurate,
    input$dev_3_1, actions$Careful, actions$Accurate,

  )


  devs_2 <- tribble(

    ~strategy_dev, ~strategy_rev, ~strategy_meta,
    input$dev_1_2, actions$Careful, actions$Accurate,
    input$dev_2_2, actions$Careful, actions$Accurate,
    input$dev_3_2, actions$Careful, actions$Accurate,

  )


  names(values) <- names(input)

  values_1 <- values[str_detect(string = names(values), pattern = "_1$")]
  names_1 <- names(values_1) %>% str_remove(pattern = "_1$")
  names(values_1) <- names_1
  values_1$devs = devs_1
  values_1$entropy_factor = 1 + values_1$entropy_factor/100

  values_2 <- values[str_detect(string = names(values), pattern = "_2$")]
  names_2 <- names(values_2) %>% str_remove(pattern = "_2$")
  names(values_2) <- names_2
  values_2$devs = devs_2
  values_2$entropy_factor = 1 + values_2$entropy_factor/100




  simulate_and_label <- function(round, values, progress, n_executions, memo = TRUE){

    simulated <- simulate_game_simmer(values, round)

    progress$inc(1/n_executions)

    simulated

  }

  progress$set(message = "Simulating 1/2", value = 0)


  result_1 <- purrr::map_df(
    .x = 1:n_executions,
    .f = ~simulate_and_label(
      round = .x,
      values = values_1,
      progress = progress,
      n_executions = n_executions
    ) %>%
      mutate(round = .x)
  )

  progress$set(message = "Simulating 2/2", value = 0)

  result_2 <- purrr::map_df(
    .x = 1:n_executions,
    .f = ~simulate_and_label(
      round = .x,
      values = values_2,
      progress = progress,
      n_executions = n_executions
    ) %>%
      mutate(round = .x)

  )


  output_result <- list()

  output_result$result_1 <- result_1
  output_result$result_2 <- result_2

  output_result



}









