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

## Fit models
fit <- holidays |>
  model(
    Seasonal_naive = SNAIVE(Trips),
    Naive = NAIVE(Trips),
    Drift = RW(Trips ~ drift()),
    Mean = MEAN(Trips)
  )

## Check residuals
fit |>
  filter(State == "Victoria") |>
  select(Seasonal_naive) |>
  gg_tsresiduals()

augment(fit) |>
  filter(State == "Victoria", .model == "Seasonal_naive") |>
  features(.innov, ljung_box, lag = 8)

## Which model fits best?

accuracy(fit) |>
  summarise(
    RMSSE = sqrt(mean(RMSSE^2)),
    MAPE = mean(MAPE),
    .by = .model
  ) |>
  arrange(RMSSE)

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
