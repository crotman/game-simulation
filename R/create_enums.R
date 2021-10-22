players <- list(
  D1 = "D1",
  D2 = "D2",
  R = "R"
)


devs <- players[names(players) %in% c("D1", "D2")]

tasks_types <- list(
  Review = "Review",
  MetaReview = "MetaReview"
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



