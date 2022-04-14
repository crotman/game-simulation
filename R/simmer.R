library(simmer)
library(tidyverse)


simulate_game_simmer <- function(
  params,
  n_execution,
  debug_mode = FALSE,
  save_db = FALSE,
  con_db = NULL,
  label = NA,
  i = NA

){



  debug_mode <- FALSE


  necessary_attributes <- c(
    "rework",
    "review_kludge",
    "first_review_kludge",
    "kludge",
    "first_kludge",
    "to_be_reviewed",
    "to_be_metareviewed",
    "developer",
    "reviewer",
    "reviewed",
    "metareviewer",
    "stage"
  )


  necessary_global_attributes <- c(
    "stage",
    "developer"
  )

#### functions ####


  players <- list(
    D1 = "D1",
    D2 = "D2",
    R = "R"
  )


  devs <- players[names(players) %in% c("D1", "D2")]

  tasks_types <- list(
    Review = "Review",
    MetaReview = "MetaReview",
    KludgePenalty = "KludgePenalty"

  )


  statuses <- list(
    Busy = "Busy",
    Idle = "Idle"
  )

  actions <- list(
    Diligent = "Diligent",
    Kludgy = "Kludgy",
    Accurate = "Accurate",
    Inaccurate = "Inaccurate",
    Careful = "Careful",
    Negligent = "Negligent"
  )


  review_statuses <- list(
    NotSampled =  "Not sampled for review",
    Waiting =  "Waiting for review",
    Kludge =  "Kludge",
    NotKludge =  "Not Kludge"
  )


  meta_review_statuses <- list(
    NotSampled =  "Not sampled for meta-review",
    Waiting =  "Waiting for meta-review",
    Positive =  "Positive",
    Negative =  "Negative"
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



  get_first_review_kludge <- function(){
    rework <- simmer::get_attribute(env, keys = "rework")
    if(rework == 0){
      simmer::get_attribute(env, keys = "review_kludge")
    }else{
      simmer::get_attribute(env, keys = "first_review_kludge")
    }
  }

  get_first_kludge <-  function(){
    rework <- simmer::get_attribute(env, keys = "rework")
    if(rework == 0){
      simmer::get_attribute(env, keys = "kludge")
    }else{
      simmer::get_attribute(env, keys = "first_kludge")
    }
  }

  set_first_kludge_review <- function(.trj){

    .trj %>%
      set_attribute_and_global(
        keys = "first_review_kludge",
        values = get_first_review_kludge
      ) %>%
      set_attribute_and_global(
        keys = "first_kludge",
        values = get_first_review_kludge
      )

  }

  add_resource_reduce <- function(a, b){
    simmer::add_resource(.env = a, name = as.character(b), preemptive = TRUE)
  }

  branch_review <- function(){
    simmer::get_attribute(env, keys = "to_be_reviewed")
  }

  branch_rework <- function(){
    simmer::get_attribute(env, keys = "review_kludge")
  }

  branch_metareview <- function(){
    to_be <- simmer::get_attribute(env, keys = "to_be_metareviewed")
    if(to_be == 1){
      saida <- 1
    }else{
      saida <-0
    }
    saida
  }

  set_attribute_and_global <- function(.trj, keys, values, mod = NULL, init = 0){

    if(is.function(values)){
      debug_stage = FALSE
    }else{
      if (values %in% c(1,8)){
        debug_stage = TRUE
      }else{
        debug_stage = FALSE
      }
    }


    if(keys %in% necessary_attributes | debug_mode){
      step <- simmer::set_attribute(.trj = .trj, keys = keys, values = values, mod = mod, init = init)


      if(keys %in% necessary_global_attributes | debug_mode){
        if(keys != "stage" | ((keys == "stage" & debug_stage) | debug_mode ) ){
             simmer::set_global(
               .trj = step,
                keys = function(){
                  paste0(simmer::get_name(env),"_", keys)
                },
                # values = function(){simmer::get_attribute(env, keys)}
                values = values
             )
           }else{
             step
           }
      }else{
        step
      }

    }else{
      .trj
    }
  }


  set_dev <- function(.trj, dev){
    dev <- str_remove(string = dev(), pattern = "developer_" ) %>%  as.integer()
    set_attribute_and_global(
      .trj = .trj,
      keys = "dev",
      values = dev
    )
  }

  get_developer_id <- function(){
    as.integer(simmer::get_selected(env))
  }

  get_reviewer_id <- function(){
    str_remove(string = simmer::get_selected(env), pattern = "reviewer_") %>% as.integer()
  }

  get_my_dev <- function(){
    simmer::get_attribute(env, 'developer')
  }

  get_my_dev_or_any <- function(){
    my_dev <- get_my_dev()
    if(is.na(my_dev)){
      as.character(1:n_developers)
    }else{
      as.character(my_dev)
    }

  }

  get_other_devs_on_review <- function(){
    current_dev <- simmer::get_attribute(env, 'developer')
    devs <- as.character((1:n_developers)[-current_dev])
    devs
  }


  get_other_devs_on_metareview <- function(){
    current_dev <- simmer::get_attribute(env, 'developer')
    current_rev <- simmer::get_attribute(env, 'reviewer')
    devs <- as.character((1:n_developers)[-c(current_dev, current_rev)])
    devs
  }

  set_time_to_develop <- function(){
    current_dev <- simmer::get_attribute(env, 'developer')
    action <- params$devs[["strategy_dev"]][current_dev]
    rework <- simmer::get_attribute(env, 'rework')
    if(rework == 0){
      mean <- pr_actions_info$mean_time_to_develop[which(pr_actions_info$action == action)]
      sd <- pr_actions_info$sd_time_to_develop[which(pr_actions_info$action == action)]
    } else{
      mean <- params$mean_time_rework
      sd <- params$sd_time_rework
    }
    entropy_factor <- params$entropy_factor ^ simmer::get_global(env, "entropy")
    rnorm(n = 1, mean = mean * entropy_factor, sd = sd * entropy_factor)
  }

  set_kludge <- function(){
    current_dev <- simmer::get_attribute(env, 'developer')
    action <- params$devs[["strategy_dev"]][current_dev]
    if(action == actions$Diligent){
      prob <- params$prob_diligent
    }else{
      prob <- params$prob_kludgy
    }
    rbinom(n = 1, size = 1, prob = prob)
  }


  set_to_be_reviewed <- function(){
    rbinom(n = 1, size = 1, prob = params$prob_review)
  }

  set_to_be_metareviewed <- function(){
    if(simmer::get_attribute(env, 'reviewed') > 0 ){
      rbinom(n = 1, size = 1, prob = params$prob_meta_review)
    }else{
      0
    }
  }


  set_time_to_review <- function(){
    rnorm(n = 1, mean = params$mean_time_review, sd = params$sd_time_review)
  }


  set_time_to_metareview <- function(){

    current_dev <- simmer::get_attribute(env, 'metareviewer')
    action <- params$devs[["strategy_meta"]][current_dev]
    if(action == actions$Accurate)
      mean <- params$mean_time_inaccurate
    else{
      mean <- params$mean_time_accurate
    }
    sd <- params$sd_time_meta_review

    rnorm(n = 1, mean = mean, sd = sd )

  }

  set_metareview <- function(){
    current_dev <- simmer::get_attribute(env, 'metareviewer')
    action <- params$devs[["strategy_meta"]][current_dev]
    kludge <- simmer::get_attribute(env, 'first_kludge')
    review_kludge <- simmer::get_attribute(env, 'first_review_kludge')
    correct_review <- kludge == review_kludge
    prob <- dplyr::case_when(
      kludge == 0 & action == actions$Accurate & !correct_review ~ params$prob_negative_when_incorrect_not_kludgy_accurate,
      kludge == 0 & action == actions$Accurate & correct_review ~ params$prob_negative_when_correct_not_kludgy_accurate,
      kludge == 0 & action == actions$Inaccurate & !correct_review ~ params$prob_negative_when_incorrect_not_kludgy_inaccurate,
      kludge == 0 & action == actions$Inaccurate & correct_review ~ params$prob_negative_when_correct_not_kludgy_inaccurate,
      kludge == 1 & action == actions$Accurate & !correct_review ~ params$prob_negative_when_incorrect_kludgy_accurate,
      kludge == 1 & action == actions$Accurate & correct_review ~ params$prob_negative_when_correct_kludgy_accurate,
      kludge == 1 & action == actions$Inaccurate & !correct_review ~ params$prob_negative_when_incorrect_kludgy_inaccurate,
      kludge == 1 & action == actions$Inaccurate & correct_review ~ params$prob_negative_when_correct_kludgy_inaccurate
    )

    rbinom(n = 1, size = 1, prob = 1-prob)

  }

  set_review <- function(){
    # current_rev <- simmer::get_attribute(env, 'reviewer') %>% str_remove("developer_") %>% as.integer()
    current_rev <- simmer::get_attribute(env, 'reviewer')
    action <- params$devs[["strategy_rev"]][current_rev]
    kludge <- simmer::get_attribute(env, 'kludge')
    prob <- dplyr::case_when(
      kludge == 1 & action == actions$Careful ~ params$prob_kludgy_review_when_kludge_careful,
      kludge == 1 & action == actions$Negligent ~ params$prob_kludgy_review_when_kludge_negligent,
      kludge == 0 & action == actions$Careful ~ params$prob_kludgy_review_when_not_kludge_careful,
      kludge == 0 & action == actions$Negligent ~ params$prob_kludgy_review_when_not_kludge_negligent
    )
    rbinom(n = 1, size = 1, prob = prob )
  }


  set_rollback <- function(){

    review_kludge <- simmer::get_attribute(env, 'review_kludge')

    if(review_kludge == 1){
      TRUE
    } else{
      FALSE
    }

  }


  get_stage_id <- function(cur_stage, cur_phase){

    stages %>% dplyr::filter(
      name == cur_stage,
      phase == cur_phase
    ) %>%
      dplyr::pull(id)

  }


#### simulation ####


  pr_actions_info <- tibble::tribble(
    ~action,          ~prob,               ~mean_time_to_develop,     ~sd_time_to_develop,
    actions$Diligent, params$prob_diligent, params$mean_time_diligent,  params$sd_time_develop,
    actions$Kludgy,   params$prob_kludgy,   params$mean_time_kludgy,    params$sd_time_develop
  )


  n_developers <- params$devs %>% nrow()
  n_reviewers <- params$revs %>% nrow()

  #### metareview traj ####

  metareview_traj <- simmer::trajectory("metareview_traj") %>%
    simmer::set_prioritization(c(1, 2, FALSE)) %>%
    simmer::select(get_other_devs_on_metareview, policy = "first-available" ) %>%
    simmer::timeout(0.0001) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "MetaReview", cur_phase = "Start")) %>%
    set_attribute_and_global(keys = "metareviewer", values = get_developer_id ) %>%
    simmer::seize_selected(1) %>%
    simmer::timeout(0.0005) %>%
    set_attribute_and_global(keys = "seized", values = 1) %>%
    set_attribute_and_global(keys = "seized_who", values = get_developer_id) %>%
    simmer::timeout(set_time_to_metareview) %>%
    set_attribute_and_global(keys = "metareview_good", values = set_metareview) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "MetaReview", cur_phase = "End")) %>%
    simmer::release_selected(1) %>%
    simmer::timeout(0.0005) %>%
    set_attribute_and_global(keys = "seized", values = 0) %>%
    simmer::timeout(0.0001) %>%
    set_attribute_and_global(keys = "seized_who", values = 0) %>%
    simmer::set_prioritization(c(0, 0, FALSE))



#### review traj ####
  review_traj <- simmer::trajectory("review_traj") %>%
    simmer::set_prioritization(c(1, 2, FALSE)) %>%
    simmer::select(get_other_devs_on_review, policy = "first-available" ) %>%
    set_attribute_and_global(keys = "reviewer", values = get_developer_id ) %>%
    simmer::timeout(0.0001) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "Review", cur_phase = "Start")) %>%
    simmer::seize_selected(1) %>%
    simmer::timeout(0.0005) %>%
    set_attribute_and_global(keys = "seized", values = 1) %>%
    set_attribute_and_global(keys = "seized_who", values = get_developer_id) %>%
    simmer::timeout(set_time_to_review) %>%
    simmer::release_selected(1) %>%
    set_attribute_and_global(keys = "review_kludge", values = set_review) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "Review", cur_phase = "End")) %>%
    simmer::timeout(0.0001) %>%
    set_attribute_and_global(keys = "seized", values = 0) %>%
    simmer::timeout(0.0001) %>%
    set_attribute_and_global(keys = "seized_who", values = 0) %>%
    set_first_kludge_review() %>%
    set_attribute_and_global(
      keys = "reviewed",
      mod = "+",
      init = 0,
      values = 1
    ) %>%
    simmer::set_prioritization(c(0, 0, FALSE))





#### development traj ####

  development_traj <- simmer::trajectory("development_traj") %>%
    simmer::set_global(
      keys = "entropy",
      values = 0,
      mod = "+",
      init = 0
    ) %>%
    set_attribute_and_global(
      keys = "rework",
      mod = "+",
      init = -1,
      values = 1
    ) %>%
    set_attribute_and_global(
      keys = "reviewed",
      mod = "+",
      init = 0,
      values = 0
    ) %>%
    simmer::select(get_my_dev_or_any, policy = "shortest-queue" ) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "Development", cur_phase = "Start")) %>%
    set_attribute_and_global(keys = "developer", values = get_developer_id ) %>%
    simmer::seize_selected(1) %>%
    simmer::timeout(0.0005) %>%
    set_attribute_and_global(keys = "seized", values = 1) %>%
    set_attribute_and_global(keys = "seized_who", values = get_developer_id) %>%
    simmer::timeout(task = set_time_to_develop ) %>%
    set_attribute_and_global(keys = "kludge", values = set_kludge ) %>%
    set_attribute_and_global(keys = "review_kludge", values = -1 ) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "Development", cur_phase = "End")) %>%
    simmer::release_selected(1) %>%
    simmer::timeout(0.0001) %>%
    set_attribute_and_global(keys = "seized", values = 0) %>%
    set_attribute_and_global(keys = "to_be_reviewed", values = set_to_be_reviewed) %>%
    simmer::timeout(0.0001) %>%
    set_attribute_and_global(keys = "seized_who", values = 0) %>%
    simmer::branch(
      option = branch_review,
      review_traj,
      continue = TRUE
    ) %>%
    simmer::timeout(0.0001) %>%
    simmer::rollback(
      amount = 100,
      check = set_rollback
    ) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "Merge", cur_phase = "Start")) %>%
    simmer::timeout(0.0001) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "Merge", cur_phase = "End")) %>%
    set_attribute_and_global(keys = "merged", values = 1) %>%
    set_attribute_and_global(keys = "to_be_metareviewed", values = set_to_be_metareviewed) %>%
    simmer::branch(
      option = branch_metareview,
      metareview_traj,
      continue = TRUE
    ) %>%
    simmer::set_global(
      keys = "entropy",
      mod = "+",
      init = 0,
      values = function(){simmer::get_attribute(env, keys = "kludge")}
    ) %>%
    identity()

  env <- simmer::simmer("simulacao")

  distr <- function() rexp(n = 1, rate = 1/params$lambda)

  env_run <- env %>%
    purrr::reduce(
    .init = .,
    .x = 1:n_developers,
    .f = add_resource_reduce
    ) %>%
    # reduce(
    #   .init = .,
    #   .x = str_glue("reviewer_{1:n_reviewers}"),
    #   .f = simmer::add_resource_reduce
    # ) %>%
    simmer::add_generator(
      name_prefix = "pr",
      trajectory = development_traj,
      distribution = simmer::from_to(0, stop_time = params$total_steps, dist = distr )
      # distribution = simmer::at( 1  )
    ) %>%
    simmer::run(until = params$total_steps)


  monitored <- simmer::get_mon_attributes(env_run)

  entropy <- monitored  %>%
    dplyr::filter(
      key == "entropy"
    )



  data <- monitored %>%
    dplyr::filter(
      !key %in% ("entropy")
    )

  # browser()

  # data <- data %>%
  #   mutate(
  #     pullrequest = str_extract(string = key, pattern = "pullrequest[0-9]*"),
  #     field = str_extract(string = key, pattern = "(?:_).*") %>% str_remove("_"),
  #     .before = value
  #   ) %>%
  #   select(-key)
  #
  #
  # data <- data %>%
  #   tidyr::pivot_wider(
  #     names_from = field,
  #     values_from = value
  #   )
  #
  #
  # browser()
  #
  # data <- data %>%
  #   dplyr::arrange(time) %>%
  #   dplyr::group_by(pullrequest) %>%
  #   tidyr::fill(
  #     dplyr::everything(),
  #     .direction = "down"
  #   )
  #
  # data <- data %>%
  #   dplyr::ungroup() %>%
  #   dplyr::left_join(
  #     stages %>% dplyr::rename(cur_stage = name),
  #     by = c("stage" = "id")
  #   )
  #
  # data <- data %>%
  #   dplyr::mutate(
  #     dplyr::across(
  #       .cols = where(is.numeric) & !time,
  #       .fns = as.integer
  #     )
  #   )


  rm(list = ls()[!(ls() %in% c("data", "entropy", "debug_mode"))   ])



  # if(!debug_mode){
    output <- data
  # }else{
  #   output <- list(
  #     data = data,
  #     entropy = entropy
  #   )
  # }

  output


}

