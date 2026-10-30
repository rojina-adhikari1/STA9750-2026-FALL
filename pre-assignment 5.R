library(nycflights13)
library(dplyr)

flights |>
  filter(origin == "JFK", dest %in% c("LGB", "HNL", "ANC", "SJC", "EWR")) |>
  distinct(dest, distance) |>
  arrange(desc(distance))

flights |>
  filter(origin == "EWR", dest %in% c("LGB", "HNL", "ANC", "SJC", "JFK")) |>
  distinct(dest, distance) |>
  arrange(desc(distance))


airports |> filter(faa == "IPL") |> select(faa, name, alt)

airports |> filter(alt > 5280) |> nrow()

airports |>
  filter(faa %in% c("ASE", "DEN", "TVL", "TEX")) |>
  select(faa, name, alt) |>
  arrange(desc(alt))

binomial(link = "log")       # works
binomial(link = "identity")  # errors: link "identity" not available for binomial family

round(sqrt(6 * trigamma(1)), 6)
unchop

flights |>
  inner_join(airports, join_by(dest == faa)) |>
  filter(tzone == "America/Los_Angeles") |>
  count(hour, sort = TRUE)


flights |>
  inner_join(airports, join_by(dest == faa)) |>
  filter(alt < 10) |>
  nrow()

flights |>
  filter(origin %in% c("JFK", "LGA", "EWR"), dest %in% c("JFK", "LGA", "EWR")) |>
  left_join(airlines, join_by(carrier == carrier))