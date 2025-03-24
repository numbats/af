library(fpp3)

aus_production
?aus_production

aus_production |>
  autoplot(Bricks)
aus_production |>
  gg_season(Bricks)
aus_production |>
  gg_subseries(Bricks)

# Power transformations (box-cox)
# ONLY works when the VARIATION
# is *PROPORTIONATE* to the LEVEL
# of the series
canadian_gas |>
  features(Volume, guerrero)

canadian_gas |>
  autoplot(Volume)

canadian_gas |>
  gg_subseries(Volume)
canadian_gas |>
  model(STL(Volume)) |>
  components() |>
  gg_season(season_year)
canadian_gas |>
  model(STL(Volume)) |>
  components() |>
  gg_subseries(season_year)

canadian_gas |>
  model(STL(Volume)) |>
  components() |>
  as_tsibble() |>
  autoplot(trend)
