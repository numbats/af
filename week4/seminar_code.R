library(fpp3)

## Apple stock price

apple <- gafa_stock |> filter(Symbol == "AAPL")

apple |> 
  autoplot(Close)

fit <- apple |>
  model(
    Seasonal_naive = SNAIVE(Close),
    Naive = NAIVE(Close),
    Drift = RW(Close ~ drift()),
    Mean = MEAN(Close)
  )

## ---- Holiday tourism by state --------------

holidays <- tourism |>
  as_tibble() |>
  filter(Purpose == "Holiday") |>
  summarise(Trips = sum(Trips), .by = c("State", "Quarter")) |>
  as_tsibble(index = Quarter, key = State)

holidays |> autoplot(Trips)

## Fit models
fit <- holidays |>
  model(
    Seasonal_naive = SNAIVE(Trips),
    Naive = NAIVE(Trips),
    Drift = RW(Trips ~ drift()),
    Mean = MEAN(Trips),
    tslm = TSLM(Trips ~ trend() + season())
  )

## Produce forecasts

holidays_fc <- fit |>
  forecast(h = "4 years")

holidays_fc |>
  autoplot(holidays, level = NULL)

holidays_fc |>
  filter(.model == "Seasonal_naive") |>
  autoplot(holidays, show_gap = FALSE)

holidays_fc |>
  hilo(level = 95) |>
  mutate(
    lower = `95%`$lower,
    upper = `95%`$upper
  )

# Forecasting with a decomposition

my_dcmp_spec <- decomposition_model(
  STL(Trips),
  NAIVE(season_adjust), # Model for seasonally adjusted series
  SNAIVE(season_year)   # Model for seasonal component
)

fit <- holidays |>
  model(stl_naive = my_dcmp_spec)


# Forecasting with a transformation

fc <- holidays |>
  model(TSLM(sqrt(Trips) ~ trend() + season())
  ) |> 
  forecast(h="5 years")

fc |> 
  filter(State == "Western Australia") |>
  autoplot(holidays)

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
