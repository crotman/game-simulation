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
  time_to_develop,
  review_status,
  meta_review_status
){

  new_pull_request <- tibble::tibble(
    developer = developer,
    kludge = kludge,
    time_to_develop = time_to_develop,
    review_status = review_status,
    meta_review_status = meta_review_status
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

  bind_rows(pull_requests, new_pull_request)
  
}


#' Title
#'
#' @param task_type 
#' @param earliest_time 
#' @param player 
#'
#' @return
#' @export
#'
#' @examples
create_task <- function(
  task_type,
  earliest_time,
  player  
){

  new_task <- tibble::tibble(
    task_type = task_type,
    earliest_time = earliest_time,
    player = player
  ) 
  
}


#' Title
#'
#' @param task_backlog 
#' @param task_type 
#' @param time_from_now 
#' @param now 
#'
#' @return
#' @export
#'
#' @examples
add_task <- function(
  task_backlog,
  task_type,
  time_from_now,
  now
){

  new_backlog <-  structtibble::sorted_bind_rows(
    sorted_tibble = create_task(task_type = task_type, earliest_time = time + time + from_now),
    binding_tibble = new_event
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
    earliest_time = integer(),
    type = integer(),
    player = integer()
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
get_dev_status <-  function(
  developers_status,
  developer
){
  developers_status %>% filter(player == developer) %>% pull(status)
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





