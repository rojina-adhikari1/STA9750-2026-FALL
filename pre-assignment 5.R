library(nycflights13)
library(dplyr)

flights |>
  filter(origin == "JFK", dest %in% c("LGB", "HNL", "ANC", "SJC", "EWR")) |>
  distinct(dest, distance) |>
  arrange(desc(distance))