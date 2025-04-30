library(fpp3)


## EGYPTIAN EXPORTS

global_economy |>
  filter(Code == "EGY") |>
  gg_tsdisplay(Exports, plot_type = "partial") +
  labs(y = "% of GDP", title = "Egyptian Exports")

fit <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports ~ pdq(4, 0, 0)))

report(fit)
gg_tsresiduals(fit)

fit |>
  forecast(h = 50) |>
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")


fit <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports))

report(fit)
gg_tsresiduals(fit)

fit |>
  forecast(h = 50) |>
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")


## NORWEGIAN IMPORTS

global_economy |>
  filter(Code == "NOR") |>
  gg_tsdisplay(Imports, plot_type = "partial") +
  labs(y = "% of GDP", title = "Norwegian Imports")

global_economy |>
  filter(Code == "NOR") |>
  gg_tsdisplay(difference(Imports), plot_type = "partial") +
  labs(y = "% of GDP", title = "Norwegian Imports")

fit <- global_economy |>
  filter(Code == "NOR") |>
  model(
    ma2 = ARIMA(Imports ~ pdq(0, 1, 2)),
    ar4 = ARIMA(Imports ~ pdq(4, 1, 0))
  )
glance(fit)

fit |> select(ma2) |> report()

fit |> select(ma2) |> gg_tsresiduals()

fit |>
  forecast(h = 50) |>
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Norwegian Imports")


fit <- global_economy |>
  filter(Code == "NOR") |>
  model(
    auto = ARIMA(Imports),
    tryhard = ARIMA(
      Imports,
      stepwise = FALSE,
      approximation = FALSE,
      order_constraint = p + q <= 10
    )
  )


## pelt::Lynx

pelt |>
  autoplot(Lynx)

# Use a sqrt transformation to keep forecasts positive

pelt |>
  autoplot(sqrt(Lynx))
pelt |>
  gg_tsdisplay(sqrt(Lynx), plot_type = "partial")

fit <- pelt |>
  model(
    auto = ARIMA(sqrt(Lynx)),
    ar2 = ARIMA(sqrt(Lynx) ~ pdq(2, 0, 0)),
    ar3 = ARIMA(sqrt(Lynx) ~ pdq(3, 0, 0)),
    tryhard = ARIMA(
      sqrt(Lynx),
      stepwise = FALSE,
      approximation = FALSE,
      order_constraint = p + q <= 10
    )
  )
fit
glance(fit) |>
  arrange(AICc)
fit |>
  select(auto) |>
  gg_tsresiduals()

fit |>
  forecast(h = 80) |>
  autoplot(pelt, level = NULL) +
  labs(y = "Number of Lynx", title = "Lynx Forecast")

fit |>
  select(auto) |>
  forecast(h = 20) |>
  autoplot(pelt) +
  labs(y = "Number of Lynx", title = "Lynx Forecast")


# Extract phi coefficients from best model
phi <- fit |>
  select(tryhard) |>
  tidy() |>
  pull(estimate) |>
  head(2)
# Is it cyclic?
phi[1]^2 + 4 * phi[2]
# Average length of cycle
2 * pi / (acos(-phi[1] * (1 - phi[2]) / (4 * phi[2])))
