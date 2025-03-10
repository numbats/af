
# Daily data: OTexts page views

otexts_views
otexts_views |> autoplot()
otexts_views |> gg_season(Pageviews)
otexts_views |> gg_season(Pageviews, period = "week")

## Scatterplots

aus_production

aus_production |> autoplot(Cement)

aus_production |>
  pivot_longer(-Quarter, values_to = "value", names_to = "Commodity") |>
  autoplot(value)

aus_production |>
  pivot_longer(-Quarter, values_to = "value", names_to = "Commodity") |>
  autoplot(value) +
  facet_grid(Commodity ~ ., scales = "free_y")

aus_production |>
  as_tibble() |>
  select(-Quarter) |>
  GGally::ggpairs()

## Cyclic data

as_tsibble(sunspot.year) |>
  autoplot(value)

as_tsibble(sunspot.year) |> gg_lag(value, lags = 1:16, geom = "point")
as_tsibble(sunspot.year) |> ACF() |> print(n=Inf)
as_tsibble(sunspot.year) |> ACF() |> autoplot()


## White noise
wn <- tsibble(t = 1:100, y = rnorm(100), index = t)
wn |> autoplot(y)
wn |> ACF(y) |> autoplot()

## Random walks
rw <- tsibble(t = 1:100, y = cumsum(rnorm(100)), index = t)
rw |> autoplot(y)
rw |> ACF(y) |> autoplot()
