library(fpp3)

## Apple stock price

apple <- gafa_stock |> filter(Symbol == "AAPL")

apple |>
  autoplot(Close)

# Use trading day as index
apple <- apple |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index = trading_day, regular = TRUE)

fit <- apple |>
  model(
    #Seasonal_naive = SNAIVE(Close),
    Naive = NAIVE(Close),
    Drift = RW(Close ~ drift()),
    Mean = MEAN(Close)
  )

fc <- fit |>
  forecast(h = 20)

fc |> autoplot(apple |> tail(50), level = NULL)


# Use date as index
apple <- apple |>
  update_tsibble(index = Date, regular = TRUE) |>
  fill_gaps()

fit <- apple |>
  model(
    Seasonal_naive = SNAIVE(Close),
    Naive = NAIVE(Close),
    Drift = RW(Close ~ drift()),
    Mean = MEAN(Close)
  )

fc <- fit |>
  forecast(h = 20)

fc |> autoplot(apple |> tail(50), level = NULL)

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

fit |> select(Mean) |> filter(State == "Victoria") |> report()

## Produce forecasts

holidays_fc <- fit |>
  forecast(h = "4 years")

holidays_fc |>
  autoplot(holidays, level = NULL)

holidays_fc |>
  filter(.model == "Seasonal_naive", State == "Victoria") |>
  autoplot(holidays, show_gap = FALSE)

holidays_fc |>
  hilo(level = 95) |>
  mutate(
    lower = `95%`$lower,
    upper = `95%`$upper
  )

