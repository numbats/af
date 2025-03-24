library(fpp3)

holidays <- tourism |>
  as_tibble() |>
  filter(Purpose == "Holiday") |>
  summarise(Trips = sum(Trips), .by = c("State", "Quarter")) |>
  as_tsibble(index = Quarter, key = State)

# Forecasting with a transformation

fc <- holidays |>
  model(TSLM(sqrt(Trips) ~ trend() + season())
  ) |>
  forecast(h="5 years")

fc |>
  filter(State == "Western Australia") |>
  autoplot(holidays)

# Forecasting with a decomposition

my_dcmp_spec <- decomposition_model(
  STL(Trips),
  NAIVE(season_adjust), # Model for seasonally adjusted series
  SNAIVE(season_year)   # Model for seasonal component
)

fit <- holidays |>
  model(stl_naive = my_dcmp_spec)


# Forecasting with a transformation and a decomposition

my_dcmp_spec <- decomposition_model(
  STL(sqrt(Trips)),
  NAIVE(season_adjust), # Model for seasonally adjusted series
  SNAIVE(season_year)   # Model for seasonal component
)

fc <- holidays |>
  model(stl_naive = my_dcmp_spec) |>
  forecast(h="5 years")

fc |>
  filter(State == "Victoria") |>
  autoplot(holidays)
