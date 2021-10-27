simulate_game <- function(
  values
){

  #initializing structures that depend on user input, therefore the reactive environment

  players_tasks <- list(
    create_backlog(),
    create_backlog(),
    create_backlog()
  )

  names(players_tasks) <- players

  pull_requests <- create_pull_requests()
  cur_time <- 0

  cum_kludges <- 0

  next_task <- NULL

  players_status <- tribble(
    ~player,     ~status,       ~release,
    players$D1,  statuses$Idle, 0,
    players$D2,  statuses$Idle, 0,
    players$R,  statuses$Idle,  0
  )


  total_time <- values$total_time

  pr_actions_info <- tribble(
    ~action,          ~prob,               ~mean_time_to_develop,     ~sd_time_to_develop,
    actions$Diligent, values$prob_diligent, values$mean_time_diligent,  values$sd_time_develop,
    actions$Kludgy,   values$prob_kludgy,   values$mean_time_kludgy,    values$sd_time_develop
  )

  devs_actions <- set_devs_actions(
    d1 = values$action_d1,
    d2 = values$action_d2
  )

  rev_action <- values$action_r

  while(cur_time < total_time){

    for(player in players){

      if(get_player_status(players_status, player) == statuses$Busy & get_player_release(players_status, player) < cur_time){
        players_status <- set_player_status(players_status, player, statuses$Idle)
      }

      player_tasks <- players_tasks[[player]]

      #if there's an ready task and player is not busy
      if(nrow(player_tasks) > 0){

        if(cur_time > player_tasks %>% slice(1) %>% pull(earliest_time) & get_player_status(players_status, player) == statuses$Idle ){

          popped <- pop_tibble(player_tasks)
          next_task <- popped$x
          players_tasks <- set_player_tasks(players_tasks, player, popped$tibble)
          related_pr <- pull_requests %>% filter(pull_request_id == next_task$pull_request_id )


          #kludge penalty
          if(next_task$type == tasks_types$KludgePenalty){
            players_status <- set_player_status(players_status, player, statuses$Busy  )
            players_status <- set_player_release(
              players_status = players_status,
              cur_player = player,
              new_release = cur_time + values$time_penalty
            )

          }

          #review
          if(next_task$type == tasks_types$Review){
            prob_review_kludge <- case_when(
              related_pr$kludge & rev_action == actions$Careful ~ values$prob_kludgy_review_when_kludge_careful,
              !related_pr$kludge & rev_action == actions$Careful ~ values$prob_kludgy_review_when_not_kludge_careful,
              related_pr$kludge & rev_action == actions$Negligent ~ values$prob_kludgy_review_when_kludge_negligent,
              !related_pr$kludge & rev_action == actions$Negligent ~ values$prob_kludgy_review_when_not_kludge_negligent
            )

            cum_kludges <- cum_kludges + related_pr$kludge

            review_result = sample(
              x = c(review_statuses$Kludge, review_statuses$NotKludge ),
              prob = c(prob_review_kludge, 1 - prob_review_kludge),
              size = 1
            )

            if(review_result == review_statuses$Kludge){

              task_meta <- create_task(
                type = tasks_types$KludgePenalty,
                earliest_time = cur_time,
                player = related_pr$developer,
                pull_request_id = related_pr$pull_request_id
              )

              player_tasks <- add_task(players_tasks[[related_pr$developer]], new_task = task_meta)
              players_tasks <- set_player_tasks(
                players_tasks = players_tasks,
                player = related_pr$developer,
                player_tasks = player_tasks
              )

            }


            pull_requests <- set_pr_review_status(
              pull_requests,
              review_result,
              related_pr$pull_request_id
            )

            time_to_review <- rtruncnorm(n = 1, mean = values$mean_time_review, sd = values$sd_time_review, a = 0, b = Inf)

            players_status <- set_player_status(players_status, player, statuses$Busy  )
            players_status <- set_player_release(
              players_status = players_status,
              cur_player = player,
              new_release = cur_time + time_to_review
            )

            if(related_pr$meta_review_status == meta_review_statuses$Waiting){
              developer_meta <- if_else(related_pr$developer == players$D1, players$D2, players$D1)

              task_meta <- create_task(
                type = tasks_types$MetaReview,
                earliest_time = cur_time + time_to_review,
                player = developer_meta,
                pull_request_id = related_pr$pull_request_id
              )

              player_tasks <- add_task(players_tasks[[developer_meta]], new_task = task_meta)
              players_tasks <- set_player_tasks(
                players_tasks = players_tasks,
                player = developer_meta,
                player_tasks = player_tasks
              )

            }
          }

          #meta_review

          if(next_task$type == tasks_types$MetaReview){
            cur_player <- player
            kludgy <- related_pr$kludge
            action <- devs_actions %>% filter(player == cur_player) %>% pull(meta_review)
            correct <- (
              related_pr$review_status == review_statuses$Kludge & related_pr$kludge |
                related_pr$review_status == review_statuses$NotKludge & !related_pr$kludge
            )


            prob_meta_review_negative <- case_when(
              kludgy & action == actions$Accurate & correct ~ values$prob_negative_when_correct_kludgy_accurate,
              kludgy & action == actions$Accurate & !correct ~ values$prob_negative_when_incorrect_kludgy_accurate,
              kludgy & action == actions$Inaccurate & correct ~ values$prob_negative_when_correct_kludgy_inaccurate,
              kludgy & action == actions$Inaccurate & !correct ~ values$prob_negative_when_incorrect_kludgy_inaccurate,
              !kludgy & action == actions$Accurate & correct ~ values$prob_negative_when_correct_not_kludgy_accurate,
              !kludgy & action == actions$Accurate & !correct ~ values$prob_negative_when_incorrect_not_kludgy_accurate,
              !kludgy & action == actions$Inaccurate & correct ~ values$prob_negative_when_correct_not_kludgy_inaccurate,
              !kludgy & action == actions$Inaccurate & !correct ~ values$prob_negative_when_incorrect_not_kludgy_inaccurate,
            )

            meta_review_result = sample(
              x = c(meta_review_statuses$Negative, meta_review_statuses$Positive ),
              prob = c(prob_meta_review_negative, 1 - prob_meta_review_negative),
              size = 1
            )

            pull_requests <- set_pr_meta_review_status(
              pull_requests,
              meta_review_result,
              related_pr$pull_request_id
            )


            mean_time_meta_review <- if_else(action == actions$Accurate, values$mean_time_accurate, values$mean_time_inaccurate)

            time_to_meta_review <- rtruncnorm(n = 1, mean = mean_time_meta_review, sd = values$sd_time_meta_review, a = 0, b = Inf)

            players_status <- set_player_status(players_status, player, statuses$Busy  )
            players_status <- set_player_release(
              players_status = players_status,
              cur_player = player,
              new_release = cur_time + time_to_meta_review
            )
          }
        }
      }

      #develop
      if(player %in% c(players$D1, players$D2) & get_player_status(players_status, player) == statuses$Idle ){



        cur_player <- player #a kludge!
        cur_action <- devs_actions %>% filter(player == cur_player) %>% pull(pull_request)
        cur_pr_actions_info <- pr_actions_info %>% filter(action == cur_action)


        review_status =
          sample(
            x = c(review_statuses$NotSampled, review_statuses$Waiting),
            size = 1,
            prob = c(1 - values$prob_review, values$prob_review )
          )



        time_to_develop <- rtruncnorm(
          n = 1,
          mean = cur_pr_actions_info$mean_time_to_develop,
          sd = cur_pr_actions_info$sd_time_to_develop,
          a = 0,
          b = Inf
        ) * values$entropy_factor ^ cum_kludges

        draw_kludge <- rbinom(prob = cur_pr_actions_info$prob, n = 1, size = 1)

        new_pr <- create_pull_request(
          developer = player,
          kludge = draw_kludge,
          time_to_develop = time_to_develop,
          start_time = cur_time,
          review_status = review_status,
          meta_review_status = case_when(
            review_status == review_statuses$NotSampled ~ meta_review_statuses$NotSampled,
            review_status == review_statuses$Waiting ~
              sample(
                x = c(meta_review_statuses$NotSampled, meta_review_statuses$Waiting ),
                size = 1,
                prob = c(1 - values$prob_meta_review, values$prob_meta_review )
              )
          )
        )



        pull_requests <- add_pull_request(pull_requests, new_pr)

        players_status <- set_player_status(players_status, player, new_status = statuses$Busy)
        players_status <- set_player_release(players_status, player, new_release = cur_time + time_to_develop)


        if(new_pr$review_status == review_statuses$Waiting){

          task_review <- create_task(
            type = tasks_types$Review,
            earliest_time = cur_time + time_to_develop,
            player = players$R,
            pull_request_id = max(pull_requests$pull_request_id)
          )

          player_tasks <- add_task(players_tasks[[players$R]], new_task = task_review)

          players_tasks <- set_player_tasks(
            players_tasks = players_tasks,
            player = players$R,
            player_tasks = player_tasks
          )


        }


      }

    }

    cur_time <- cur_time + 1

  }


  pull_requests


}
