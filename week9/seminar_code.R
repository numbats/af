library(fpp3)

## EGYPTIAN EXPORTS

global_economy |>
  filter(Code == "EGY") |>
  autoplot(Exports) +
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

global_economy |>
  filter(Code == "EGY") |>
  ACF(Exports) |>
  autoplot()
global_economy |>
  filter(Code == "EGY") |>
  PACF(Exports) |>
  autoplot()

global_economy |>
  filter(Code == "EGY") |>
  gg_tsdisplay(Exports, plot_type = "partial")

fit1 <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports ~ pdq(4, 0, 0)))

report(fit1)


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
    ar2 = ARIMA(sqrt(Lynx) ~ pdq(2,0,0)),
    ar3 = ARIMA(sqrt(Lynx) ~ pdq(3,0,0)),
    tryhard = ARIMA(sqrt(Lynx), stepwise = FALSE,
                    approximation = FALSE,
                    order_constraint = p + q <= 10)
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
phi[1]^2 + 4*phi[2]
# Average length of cycle
2*pi / (acos(-phi[1] * (1 - phi[2])/(4*phi[2])))
