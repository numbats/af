library(fpp3)

tobacco <- aus_tobacco |>
  summarise(Expenditure = sum(Expenditure))
tobacco |> autoplot(Expenditure)


tobacco |>
  model(
    stl = STL(log(Expenditure) ~ season(window = 21) +
                trend(window = 13))
  ) |>
  components() |>
  autoplot()

fit <- tobacco |>
  model(
    ets = ETS(Expenditure),
    arima = ARIMA(log(Expenditure)),
    stl_arima = decomposition_model(
      STL(log(Expenditure) ~ season(window = 21) +
            trend(window = 13)),
      ARIMA(season_adjust),
      SNAIVE(season_year)
    )
  )

fit |> select(ets) |> report()
fit |> select(arima) |> report()
fit |> select(stl_arima) |> report()


fit |> select(ets) |> gg_tsresiduals()
fit |> select(arima) |> gg_tsresiduals()
fit |> select(stl_arima) |> gg_tsresiduals()


fit |> accuracy()


fit |>
  forecast(h = 24) |>
  autoplot(tobacco |> filter(year(Quarter) >= 2010),
           level = 80, alpha = 0.5
  )


fit |>
  select(arima) |>
  forecast(h = 1) |>
  mutate(
    .median = median(Expenditure),
    PI = hilo(Expenditure, level = 95)
  )
fit |> select(arima) |> report()

