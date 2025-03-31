
## hh_budget exercise

# 1. Create training set by withholding last four years
train <- hh_budget |>
  filter(Year <= max(Year) - 4)
#2. Fit benchmarks
fit <- train |>
  model(
    naive = NAIVE(Wealth),
    drift = RW(Wealth ~ drift()),
    mean = MEAN(Wealth)
  )
fc <- fit |> forecast(h = 4)

# 3. Compute accuracy
fc |>
  accuracy(hh_budget) |>
  arrange(Country, RMSE)
fc |>
  accuracy(hh_budget) |>
  summarise(RMSE = sqrt(mean(RMSE^2)), .by=.model) |>
  arrange(RMSE)

# 4. Do the residuals resemble white noise?

fit |>
  filter(Country == "Australia") |>
  select(drift) |>
  gg_tsresiduals()
fit |>
  filter(Country == "Canada") |>
  select(drift) |>
  gg_tsresiduals()
fit |>
  filter(Country == "Japan") |>
  select(drift) |>
  gg_tsresiduals()
fit |>
  filter(Country == "USA") |>
  select(drift) |>
  gg_tsresiduals()
