library(tidyverse)
library(here)

command <- paste(
  sep = " ",
  here("gambit/gambit-enumpure.exe"),
  here("prisoners/prisoner.nfg")
)

strategies <- 2

developers <- 2

output <- system(command =  command, intern = TRUE) %>%
  last() %>%
  str_split(
    pattern = ","
  ) %>%
  enframe() %>%
  unnest(value) %>%
  filter(
    value != "NE"
  ) %>%
  mutate(
    index = row_number() - 1
  ) %>%
  mutate(
    player = index %/% developers + 1,
    strategy = index %% strategies + 1
  ) %>%
  filter(
    value == 1
  ) %>%
  select(
    player, strategy
  )

