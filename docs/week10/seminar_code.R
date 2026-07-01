library(fpp3)

## US leisure employment

leisure <- us_employment |>
  filter(
    Title == "Leisure and Hospitality",
    year(Month) > 2000
  ) |>
  mutate(Employed = Employed / 1000) |>
  select(Month, Employed)
leisure |>
  autoplot(Employed) +
  labs(
    title = "US employment: leisure and hospitality",
    y = "Number of people (millions)"
  )

leisure |>
  gg_tsdisplay(difference(Employed, 12), plot_type = "partial", lag = 36) +
  labs(title = "Seasonally differenced", y = "")

leisure |>
  gg_tsdisplay(
    difference(Employed, 12) |> difference(),
    plot_type = "partial",
    lag = 36
  ) +
  labs(title = "Double differenced", y = "")

leisure_fit <- leisure |>
  model(
    arima012011 = ARIMA(Employed ~ pdq(0, 1, 2) + PDQ(0, 1, 1)),
    arima210011 = ARIMA(Employed ~ pdq(2, 1, 0) + PDQ(0, 1, 1)),
    auto = ARIMA(Employed),
    best = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
leisure_fit
leisure_fit |>
  pivot_longer(everything(), names_to = "Model name", values_to = "Orders")

glance(leisure_fit) |>
  arrange(AICc) |>
  select(.model:BIC)

leisure_fit |>
  select(best) |>
  report()

leisure_fit |>
  select(best) |>
  gg_tsresiduals(lag = 36)

leisure_fit |>
  select(best) |>
  report()
augment(leisure_fit) |>
  filter(.model == "best") |>
  features(.innov, ljung_box, lag = 24, dof = 4)

forecast(leisure_fit, h = 36) |>
  filter(.model == "best") |>
  autoplot(leisure) +
  labs(
    title = "US employment: leisure and hospitality",
    y = "Number of people (millions)"
  )


# QUARTERLY CEMENT ETS vs ARIMA

cement <- aus_production |>
  select(Cement) |>
  filter_index("1988 Q1" ~ .)
cement |> autoplot(Cement)

# Fit ETS and ARIMA models to a training set and compare forecasts on test set
train <- cement |> filter_index(. ~ "2007 Q4")
fit <- train |>
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement)
  )
# Residual diagnostic for ETS model
fit |>
  select(ets) |>
  report()
gg_tsresiduals(fit |> select(ets), lag_max = 16)
fit |>
  select(ets) |>
  augment() |>
  features(.innov, ljung_box, lag = 8)
# Residual diagnostic for ARIMA model
fit |>
  select(arima) |>
  report()
gg_tsresiduals(fit |> select(arima), lag_max = 16)
fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 8, dof = 5)

# Forecasts
fc <- fit |>
  forecast(h = "3 years")
fc |>
  autoplot(cement) +
  labs(
    title = "Cement production in Australia",
    y = "Tonnes ('000)"
  )
fc |> accuracy(cement)

# Compare models using tscv

cement_stretch <- cement |>
  stretch_tsibble(.init = 24, .step = 1)
fit_stretch <- cement_stretch |>
  model(
    ets = ETS(Cement),
    arima = ARIMA(Cement)
  )
fc_stretch <- fit_stretch |>
  forecast(h = "2 years") |>
  group_by(.id, .model) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Cement", distribution = Cement)

fc_stretch |>
  accuracy(cement, by = c("h", ".model")) |>
  ggplot(aes(x = h, y = RMSE, color = .model)) +
  geom_line()
