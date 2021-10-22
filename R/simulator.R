#' Title
#'
#' @param developer
#' @param kludge
#' @param time_to_develop
#' @param review_status
#' @param meta_review_status
#'
#' @return
#' @export
#'
#' @examples
create_pull_request <- function(
  developer,
  kludge,
  start_time,
  time_to_develop,
  review_status,
  meta_review_status
){

  new_pull_request <- tibble::tibble(
    developer = developer,
    kludge = kludge,
    start_time = start_time,
    time_to_develop = time_to_develop,
    review_status = review_status,
    meta_review_status = meta_review_status
  )


}


set_pr_review_status <- function(
  pull_requests,
  new_review_status,
  cur_pull_request_id
){

  pull_requests %>%
    mutate(
      review_status = if_else(
        cur_pull_request_id == pull_request_id,
        new_review_status,
        review_status
      )
    )

}


set_pr_meta_review_status <- function(
  pull_requests,
  new_meta_review_status,
  cur_pull_request_id
){

  pull_requests %>%
    mutate(
      meta_review_status = if_else(
        cur_pull_request_id == pull_request_id,
        new_meta_review_status,
        meta_review_status
      )
    )

}



create_pull_requests <- function(){

  pull_requests <- tibble::tibble(
    developer = character(0),
    kludge = integer(0),
    start_time = numeric(0),
    time_to_develop = numeric(0),
    review_status = character(0),
    meta_review_status = character(0)
  ) %>%
    structtibble::sorted_tibble(
      key = start_time,
      autonumbered_id = pull_request_id
    )
}


#' Title
#'
#' @param pull_requests
#' @param developer
#' @param kludge
#' @param time_to_develop
#' @param review_status
#' @param meta_review_status
#'
#' @return
#' @export
#'
#' @examples
add_pull_request <- function(
  pull_requests = NULL,
  new_pull_request
){

  sorted_bind_rows(pull_requests, new_pull_request)

}


#' Title
#'
#' @param type
#' @param earliest_time
#' @param player
#'
#' @return
#' @export
#'
#' @examples
create_task <- function(
  type,
  earliest_time,
  player,
  pull_request_id
){

  new_task <- tibble::tibble(
    type = type,
    earliest_time = earliest_time,
    player = player,
    pull_request_id = pull_request_id
  )

}


#' Title
#'
#' @param task_backlog
#' @param type
#' @param time_from_now
#' @param now
#'
#' @return
#' @export
#'
#' @examples
add_task <- function(
  task_backlog,
  new_task
){

  new_backlog <-  structtibble::sorted_bind_rows(
    sorted_tibble = task_backlog,
    binding_tibble = new_task
  )
  new_backlog
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
create_backlog <- function(

){

  task_backlog <- tibble::tibble(
    earliest_time = numeric(),
    type = character(),
    player = character(),
    pull_request_id = integer()
  ) %>%
    structtibble::sorted_tibble(
      key = earliest_time,
      autonumbered_id = task_id
    )
}


#' Title
#'
#' @param developers_status
#' @param developer
#'
#' @return
#' @export
#'
#' @examples
get_player_status <-  function(
  players_status,
  cur_player
){
  players_status %>% filter(player == cur_player) %>% pull(status)
}

get_player_release <-  function(
  players_status,
  cur_player
){
  players_status %>% filter(player == cur_player) %>% pull(release)
}


set_player_status <-  function(
  players_status,
  cur_player,
  new_status
){
  players_status %>%
    mutate(
      status = if_else(player == cur_player, new_status, status  )
    )
}

set_player_release <-  function(
  players_status,
  cur_player,
  new_release
){
  players_status %>%
    mutate(
      release = if_else(player == cur_player, new_release, release  )
    )
}



#' Title
#'
#' @param d1
#' @param d2
#'
#' @return
#' @export
#'
#' @examples
set_devs_actions <- function(d1 = "Diligent/Accurate", d2 = "Kludgy/Inaccurate"){
  devs_actions <- tribble(
    ~player,    ~actions,
    players$D1, d1,
    players$D2, d2,
  ) %>%
    separate(
      col = actions,
      into = c("pull_request", "meta_review"),
      sep = "/"
    ) %>%
    rowwise() %>%
    mutate(
      across(
        .cols = c(pull_request, meta_review),
        .fns = ~actions[[.x]]
      )
    ) %>%
    ungroup()
}


get_action_info <- function(data, cur_action, info){

  data %>% filter(action == cur_action) %>%  pull({{info}})

}


set_player_tasks <- function(
  players_tasks,
  player,
  player_tasks
){

  players_tasks[[player]] <- player_tasks

  players_tasks

}


