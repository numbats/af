library(fpp3)

# Chinese GDP

china <- global_economy |>
  filter(Country == "China")  |>
  transmute(GDP = GDP/1e9)

china |> autoplot(GDP)
china |> autoplot(log(GDP))

china |> model(ETS(GDP)) |> report()
china |> model(ETS(GDP)) |> forecast(h=20) |> autoplot(china, level = NULL)

fit <- china |>
  model(
    ets = ETS(GDP),
    ets_damped = ETS(GDP ~ trend("Ad")),
    ets_bc = ETS(box_cox(GDP, 0.6)),
    ets_log = ETS(log(GDP))
  )

fit

augment(fit)

fit |>
  forecast(h = "20 years") |>
  autoplot(china, level = NULL) +
  scale_y_log10()

# Australian gas production

aus_production |>
  autoplot(Gas)

fit <- aus_production |>
  model(
    auto = ETS(Gas),
    hw = ETS(Gas ~ error("M") + trend("A") + season("M")),
    hwdamped = ETS(Gas ~ error("M") + trend("Ad") + season("M")),
  )
fit
fit |> glance()

fit |>
  select(hw) |>
  gg_tsresiduals()

fit |> tidy()

fit |>
  augment() |>
  filter(.model == "hw") |>
  features(.innov, ljung_box, lag = 24)

fit |>
  forecast(h = 36) |>
  filter(.model == "hw") |>
  autoplot(aus_production)

# Try fitting model to data since 1990

fit <- aus_production |>
  filter(year(Quarter) >= 1990) |>
  model(
    auto = ETS(Gas),
    hw = ETS(Gas ~ error("M") + trend("A") + season("M")),
    hwdamped = ETS(Gas ~ error("M") + trend("Ad") + season("M")),
  )
fit
fit |> glance()

fit |>
  select(auto) |>
  gg_tsresiduals()

fit |>
  augment() |>
  filter(.model == "auto") |>
  features(.innov, ljung_box, lag = 24)

fit |>
  forecast(h = 36) |>
  filter(.model == "auto") |>
  autoplot(aus_production)
