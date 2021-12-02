library(simmer)
library(tidyverse)



simulate_game_simmer <- function(
  params
){


#### functions ####


  get_first_review_kludge <- function(){
    rework <- get_attribute(env, keys = "rework")
    if(rework == 0){
      get_attribute(env, keys = "review_kludge")
    }else{
      get_attribute(env, keys = "first_review_kludge")
    }
  }

  get_first_kludge <-  function(){
    rework <- get_attribute(env, keys = "rework")
    if(rework == 0){
      get_attribute(env, keys = "kludge")
    }else{
      get_attribute(env, keys = "first_kludge")
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
    add_resource(.env = a, name = b)
  }

  branch_review <- function(){
    get_attribute(env, keys = "to_be_reviewed")
  }

  branch_rework <- function(){
    get_attribute(env, keys = "review_kludge")
  }

  branch_metareview <- function(){
    to_be <- get_attribute(env, keys = "to_be_metareviewed")
    if(to_be == 1){
      saida <- 1
    }else{
      saida <-0
    }
    saida
  }

  set_attribute_and_global <- function(.trj, keys, values, mod = NULL, init = 0){

    set_attribute(.trj = .trj, keys = keys, values = values, mod = mod, init = init) %>%
      set_global(
        keys = function(){paste0(get_name(env),"_", keys)},
        values = function(){get_attribute(env, keys)}
      )
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
    str_remove(string = get_selected(env), pattern = "developer_") %>% as.integer()
  }

  get_reviewer_id <- function(){
    str_remove(string = get_selected(env), pattern = "reviewer_") %>% as.integer()
  }

  get_my_dev <- function(){
    str_glue("developer_{get_attribute(env, 'developer')}")
  }

  get_my_dev_or_any <- function(){
    my_dev <- get_my_dev()
    if(my_dev == "developer_NA"){
      str_glue("developer_{1:n_developers}")
    }else{
      my_dev
    }

  }

  get_other_devs <- function(){
    current_dev <- get_attribute(env, 'developer')
    devs <- (1:n_developers)[-current_dev]
    str_glue("developer_{devs}")
  }

  set_time_to_develop <- function(){
    current_dev <- get_attribute(env, 'developer') %>% str_remove("developer_") %>% as.integer()
    action <- params$devs[["strategy_dev"]][current_dev]
    rework <- get_attribute(env, 'rework')
    if(rework == 0){
      mean <- pr_actions_info$mean_time_to_develop[which(pr_actions_info$action == action)]
      sd <- pr_actions_info$sd_time_to_develop[which(pr_actions_info$action == action)]
    } else{
      mean <- params$mean_time_rework
      sd <- params$sd_time_rework
    }
    entropy_factor <- params$entropy_factor ^ get_global(env, "entropy")
    rnorm(n = 1, mean = mean * entropy_factor, sd = sd * entropy_factor)
  }

  set_kludge <- function(){
    current_dev <- get_attribute(env, 'developer') %>% str_remove("developer_") %>% as.integer()
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
    if(get_attribute(env, 'reviewed') > 0 ){
      rbinom(n = 1, size = 1, prob = params$prob_meta_review)
    }else{
      0
    }
  }


  set_time_to_review <- function(){
    rnorm(n = 1, mean = params$mean_time_review, sd = params$sd_time_review)
  }


  set_time_to_metareview <- function(){

    current_dev <- get_attribute(env, 'metareviewer') %>% str_remove("developer_") %>% as.integer()
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
    current_dev <- get_attribute(env, 'metareviewer') %>% str_remove("developer_") %>% as.integer()
    action <- params$devs[["strategy_meta"]][current_dev]
    kludge <- get_attribute(env, 'first_kludge')
    review_kludge <- get_attribute(env, 'first_review_kludge')
    correct_review <- kludge == review_kludge
    prob <- case_when(
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
    current_rev <- get_attribute(env, 'reviewer') %>% str_remove("reviewer_") %>% as.integer()
    action <- params$revs[["strategy_rev"]][current_rev]
    kludge <- get_attribute(env, 'kludge')
    prob <- case_when(
      kludge == 1 & action == actions$Careful ~ params$prob_kludgy_review_when_kludge_careful,
      kludge == 1 & action == actions$Negligent ~ params$prob_kludgy_review_when_kludge_negligent,
      kludge == 0 & action == actions$Careful ~ params$prob_kludgy_review_when_not_kludge_careful,
      kludge == 0 & action == actions$Negligent ~ params$prob_kludgy_review_when_not_kludge_negligent
    )
    rbinom(n = 1, size = 1, prob = prob )
  }


  set_rollback <- function(){

    review_kludge <- get_attribute(env, 'review_kludge')

    if(review_kludge == 1){
      TRUE
    } else{
      FALSE
    }

  }


  get_stage_id <- function(cur_stage, cur_phase){

    stages %>% filter(
      name == cur_stage,
      phase == cur_phase
    ) %>%
      pull(id)

  }


#### simulation ####


  pr_actions_info <- tribble(
    ~action,          ~prob,               ~mean_time_to_develop,     ~sd_time_to_develop,
    actions$Diligent, params$prob_diligent, params$mean_time_diligent,  params$sd_time_develop,
    actions$Kludgy,   params$prob_kludgy,   params$mean_time_kludgy,    params$sd_time_develop
  )



  n_developers <- params$devs %>% nrow()
  n_reviewers <- params$revs %>% nrow()

  #### metareview traj ####

  metareview_traj <- trajectory("metareview_traj") %>%
    simmer::select(get_other_devs, policy = "random" ) %>%
    timeout(0.0001) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "MetaReview", cur_phase = "Start")) %>%
    set_attribute_and_global(keys = "metareviewer", values = get_developer_id ) %>%
    seize_selected(1) %>%
    set_attribute_and_global(keys = "seized", values = 1) %>%
    timeout(set_time_to_metareview) %>%
    set_attribute_and_global(keys = "metareview_good", values = set_metareview) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "MetaReview", cur_phase = "End")) %>%
    release_selected(1) %>%
    timeout(0.0001) %>%
    set_attribute_and_global(keys = "seized", values = 0)


#### review traj ####
  review_traj <- trajectory("review_traj") %>%
    simmer::select(str_glue("reviewer_{1:n_reviewers}"), policy = "round-robin" ) %>%
    set_attribute_and_global(keys = "reviewer", values = get_reviewer_id ) %>%
    timeout(0.0001) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "Review", cur_phase = "Start")) %>%
    seize_selected(1) %>%
    set_attribute_and_global(keys = "seized", values = 1) %>%
    timeout(set_time_to_review) %>%
    release_selected(1) %>%
    set_attribute_and_global(keys = "review_kludge", values = set_review) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "Review", cur_phase = "End")) %>%
    timeout(0.0001) %>%
    set_attribute_and_global(keys = "seized", values = 0) %>%
    set_first_kludge_review() %>%
    set_attribute_and_global(
      keys = "reviewed",
      mod = "+",
      init = 0,
      values = 1
    )




#### development traj ####

  development_traj <- trajectory("development_traj") %>%
    set_global(
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
    simmer::select(get_my_dev_or_any, policy = "random" ) %>%
    set_attribute_and_global(keys = "developer", values = get_developer_id ) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "Development", cur_phase = "Start")) %>%
    seize_selected(1) %>%
    set_attribute_and_global(keys = "seized", values = 1) %>%
    timeout(task = set_time_to_develop ) %>%
    set_attribute_and_global(keys = "kludge", values = set_kludge ) %>%
    set_attribute_and_global(keys = "review_kludge", values = -1 ) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "Development", cur_phase = "End")) %>%
    release_selected(1) %>%
    timeout(0.0001) %>%
    set_attribute_and_global(keys = "seized", values = 0) %>%
    set_attribute_and_global(keys = "to_be_reviewed", values = set_to_be_reviewed) %>%
    branch(
      option = branch_review,
      review_traj,
      continue = TRUE
    ) %>%
    timeout(0.0001) %>%
    rollback(
      amount = 100,
      check = set_rollback
    ) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "Merge", cur_phase = "Start")) %>%
    timeout(0.0001) %>%
    set_attribute_and_global(keys = "stage", values = get_stage_id(cur_stage = "Merge", cur_phase = "End")) %>%
    set_attribute_and_global(keys = "merged", values = 1) %>%
    set_attribute_and_global(keys = "to_be_metareviewed", values = set_to_be_metareviewed) %>%
    branch(
      option = branch_metareview,
      metareview_traj,
      continue = TRUE
    ) %>%
    set_global(
      keys = "entropy",
      mod = "+",
      init = 0,
      values = function(){get_attribute(env, keys = "kludge")}
    ) %>%
    identity()

  env <- simmer("simulacao")

  env_run <- env %>%
    reduce(
    .init = .,
    .x = str_glue("developer_{1:n_developers}"),
    .f = add_resource_reduce
    ) %>%
    reduce(
      .init = .,
      .x = str_glue("reviewer_{1:n_reviewers}"),
      .f = add_resource_reduce
    ) %>%
    add_generator(
      name_prefix = "pullrequest",
      trajectory = development_traj,
      distribution = at( seq(from = 1, to = params$total_steps, by = 1/params$lambda  ))
    ) %>%
    run(until = params$total_steps)


  monitored <- get_mon_attributes(env_run)


  entropy <- monitored  %>%
    filter(
      key == "entropy"
    )


  data <- monitored %>%
    filter(
      !key %in% ("entropy")
    ) %>%
    tidyr::separate(
      col = key,
      into = c("pullrequest", "field"),
      extra = "merge"
    ) %>%
    pivot_wider(
      names_from = field,
      values_from = value
    ) %>%
    arrange(time) %>%
    group_by(pullrequest) %>%
    fill(
      everything(),
      .direction = "down"
    ) %>%
    ungroup() %>%
    left_join(
      stages %>% rename(cur_stage = name),
      by = c("stage" = "id")
    )



  list(
    data = data,
    entropy = entropy
  )


}

