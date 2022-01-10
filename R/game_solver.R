library(gtree)


solve_game <- function(params, results){

  params <- readr::read_rds("values.rds")

  results <- readr::read_rds("results.rds")

  strategies <- purrr::map2_df(
    .x = params,
    .y = 1:length(params),
    .f = ~{
      .x$devs %>%
        dplyr::mutate(
          combination = .y,
          dev = row_number()
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


  # eval(parse(text = paste0('calculate_payoff <- function(player,  ...){
  #
  #
  #   dot_args <- list(...)
  #
  #
  #   ',
  #
  #   strategy_declaration,
  #
  #   "\n",
  #
  #   results_declaration,
  #
  #   '
  #
  # calculate_payoff_inside <-  function(params, strategies, results){
  #
  #   cur_params <- params %>%
  #     dplyr::mutate(
  #       dev = stringr::str_extract(name, pattern = "[0-9]*$") %>%  as.integer()
  #     )
  #
  #
  #   current_combination <- strategies  %>%
  #     dplyr::inner_join(
  #       cur_params,
  #       by = c("dev", "complete_strategy" = "value")
  #     ) %>%
  #     dplyr::group_by(
  #       combination
  #     ) %>%
  #     dplyr::filter(
  #       dplyr::n() == 3
  #     ) %>%
  #     dplyr::pull(combination) %>%
  #     dplyr::first()
  #
  #
  #   if(length(current_combination) > 0){
  #
  #     payoff <- results %>%
  #       dplyr::filter(
  #         combination == current_combination,
  #         developer == player
  #       ) %>%
  #       dplyr::pull(merges) %>%
  #       first()
  #
  #     output <- payoff
  #
  #   }else{
  #     output <- 0
  #   }
  #
  # }
  #
  #
  # output <- dot_args %>%
  #   tibble::enframe() %>%
  #   tidyr::unnest(value) %>%
  #   dplyr::group_by(
  #     name
  #   ) %>%
  #   dplyr::mutate(
  #     instance = dplyr::row_number()
  #   ) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::group_by(
  #     instance
  #   ) %>%
  #   tidyr::nest() %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(
  #     payoff = calculate_payoff_inside(params = data, strategies = strategies, results = results)
  #   ) %>%
  #   tidyr::replace_na(
  #     list(payoff = 0)
  #   ) %>%
  #   pull(payoff)
  #
  #
  # output
  #
  #
  # }')))
  #


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

                       # compute=list(
                       #   as.formula("payoff_1 ~ calculate_payoff(player = 1, testimonial_1 = testimonial_1, testimonial_2 = testimonial_2)"),
                       #   as.formula("payoff_2 ~ calculate_payoff(player = 2, testimonial_1 = testimonial_1, testimonial_2 = testimonial_2)")
                       # )
                    ))


  game_stages <- c(strategy_stages, payoff_stages)

  browser()

  game = new_game(
    gameId = "Simulation",
    options = make_game_options(verbose=TRUE),
    params = list(numPlayers=length(devs)),
    stages = game_stages
  )



  eq <- game %>%
    game_solve() %>%
    eq_tables(combine = 2)


  eq

}

