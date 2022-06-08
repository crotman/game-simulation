library(tidyverse)


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


stages <- tribble(
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






