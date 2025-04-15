library(fpp3)
aus_trips <- tourism |>
  summarise(Trips = sum(Trips))
























# 1.

aus_trips |> autoplot(Trips)
aus_trips |> gg_season(Trips)
aus_trips |> gg_subseries(Trips)



















# 2.

fit <- aus_trips |>
  model(
    ets = ETS(Trips)
  )
report(fit)
















# 3.

fc <- fit |> forecast(h = "2 years")
fc |> autoplot(aus_trips)














# 4

decomp <- aus_trips |> model(stl = STL(Trips))
decomp |> components() |> autoplot()
decomp |>
  components() |>
  as_tsibble() |>
  autoplot(season_adjust)















# 5

fit <- aus_trips |>
  model(
    dcmp = decomposition_model(
      STL(Trips),
      ETS(season_adjust ~ error("A") + trend("A") + season("N"))
    ),
    ets = ETS(Trips)
  )
fc <- fit |> forecast(h = "2 years")
fc |> autoplot(aus_trips)



















# 6
accuracy(fit)

















# 7

training <- aus_trips |>
  head(-12)

fit <- training |>
  model(
    dcmp = decomposition_model(
      STL(Trips),
      ETS(season_adjust ~ error("A") + trend("A") + season("N"))
    ),
    ets = ETS(Trips),
    snaive = SNAIVE(Trips)
  )
fc <- fit |> forecast(h = "3 years")
fc |> autoplot(aus_trips, level = NULL)




















# 8
accuracy(fc, aus_trips)





















# 9
fit |>
  select(dcmp) |>
  gg_tsresiduals()
fit |>
  select(dcmp) |>
  augment() |>
  features(.innov, ljung_box, lag = 8)
