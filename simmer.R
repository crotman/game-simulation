library(simmer)
library(tidyverse)

env <- simmer("simulacao")

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
  get_attribute(env, keys = "to_be_metareviewed")
}

set_attribute_and_global <- function(.trj, keys, values, mod = NULL, init = 0){
  set_attribute(.trj = .trj, keys = keys, values = values, mod = mod, init = init) %>%
  set_global(
    keys = function(){paste0(get_name(env),"_", keys)},
    values = values,
    mod = mod,
    init = init
  )
}


set_dev <- function(.trj, dev){
  browser()
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

n_developers <- 3
n_reviewers <- 3

metareview_traj <- trajectory("metareview_traj") %>%
  log_("metareview") %>%
  simmer::select(get_other_devs, policy = "random" ) %>%
  set_attribute_and_global(keys = "metareviewer", values = get_developer_id ) %>%
  seize_selected(1) %>%
  timeout(10) %>%
  set_attribute_and_global(keys = "metareview", values = 1) %>%
  release_selected(1)


review_traj <- trajectory("review_traj") %>%
  simmer::select(str_glue("reviewer_{1:n_reviewers}"), policy = "round-robin" ) %>%
  set_attribute_and_global(keys = "reviewer", values = get_reviewer_id ) %>%
  seize_selected(1) %>%
  timeout(10) %>%
  set_attribute_and_global(keys = "review", values = 1) %>%
  set_attribute_and_global(keys = "to_be_metareviewed", values = 1) %>%
  release_selected(1) %>%
  log_("go review") %>%
  branch(
    option = branch_metareview,
    metareview_traj,
    continue = TRUE
  )



development_traj <- trajectory("development_traj") %>%
  set_attribute_and_global(
    keys = "rework",
    mod = "+",
    init = -1,
    values = 1
  ) %>%
  simmer::select(get_my_dev_or_any, policy = "random" ) %>%
  set_attribute_and_global(keys = "developer", values = get_developer_id ) %>%
  seize_selected(1) %>%
  timeout(10) %>%
  set_attribute_and_global(keys = "kludge", values = 1) %>%
  release_selected(1) %>%
  set_attribute_and_global(keys = "to_be_reviewed", values = 1) %>%
  branch(
    option = branch_review,
    review_traj,
    continue = TRUE
  ) %>%
  rollback(
    amount = 100,
    times = 3
  )







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
    distribution = at(1:10)
  ) %>%
  run(until = 200)



get_mon_attributes(env_run) %>%
  separate(
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
  view()

