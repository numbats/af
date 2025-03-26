library(fpp3)

## ---- Holiday tourism by state --------------

holidays <- tourism |>
  as_tibble() |>
  filter(Purpose == "Holiday") |>
  summarise(Trips = sum(Trips), .by = c("State", "Quarter")) |>
  as_tsibble(index = Quarter, key = State)

## Fit models
fit <- holidays |>
  model(
    seasonal_naive = SNAIVE(Trips),
    naive = NAIVE(Trips),
    drift = RW(Trips ~ drift()),
    mean = MEAN(Trips),
    stlf = decomposition_model(
      STL(Trips),
      NAIVE(season_adjust)
    ))
  
## Check residuals
fit |>
  filter(State == "Victoria") |>
  select(seasonal_naive) |>
  gg_tsresiduals()

augment(fit) |>
  filter(State == "Victoria") |>
  features(.innov, ljung_box, lag = 8)

## Which model fits best?

accuracy(fit) |>
  summarise(
    RMSSE = sqrt(mean(RMSSE^2)),
    .by = .model
  ) |>
  arrange(RMSSE)

## Produce forecasts

holidays_fc <- fit |>
  forecast(h = "4 years")

holidays_fc |>
  autoplot(holidays, level = NULL)

holidays_fc |>
  filter(.model == "seasonal_naive") |>
  autoplot(holidays)

# Use a test set of last 2 years to check forecast accuracy

training <- holidays |>
  filter(Quarter <= max(Quarter) - 8)

training_fit <- training |>
  model(
    seasonal_naive = SNAIVE(Trips),
    naive = NAIVE(Trips),
    drift = RW(Trips ~ drift()),
    mean = MEAN(Trips),
    stlf = decomposition_model(
      STL(Trips),
      NAIVE(season_adjust)
    )
  )

test_fc <- training_fit |>
  forecast(h = "2 years")

test_fc |>
  autoplot(holidays, level = NULL)

test_fc |>
  accuracy(holidays) |>
  summarise(
    RMSSE = sqrt(mean(RMSSE^2)),
    MAPE = mean(MAPE),
    .by = .model
  ) |>
  arrange(RMSSE)

# Now use time series cross-validation to check forecast accuracy

holiday_stretch <- holidays |>
  stretch_tsibble(.init = 12, .step = 1) |>
  filter(.id != max(.id))

cv_fit <- holiday_stretch |>
  model(
    seasonal_naive = SNAIVE(Trips),
    stlf = decomposition_model(
      STL(Trips),
      NAIVE(season_adjust)
    )
  )

cv_fc <- cv_fit |>
  forecast(h = 12) |>
  group_by(.id, State, .model) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Trips", distribution = Trips)

cv_fc |>
  accuracy(holidays, by = c("h", ".model", "State")) |>
  summarise(
    RMSSE = sqrt(mean(RMSSE^2)),
    .by = c(.model, h)
  ) |>
  ggplot(aes(x=h, y=RMSSE, group=.model, col=.model)) +
  geom_line()

