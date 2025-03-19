library(fpp3)

# Transformations to stationarity

# Facebook stock
fb <- gafa_stock |>
  filter(Symbol == "FB")
fb |>
  gg_tsdisplay(Close)
fb |>
  gg_tsdisplay(difference(Close))

# aus_production Bricks

aus_production |>
  gg_tsdisplay(Bricks)
aus_production |>
  gg_tsdisplay(
    box_cox(Bricks, lambda = 0.3)
  )
aus_production |>
  gg_tsdisplay(
    box_cox(Bricks, lambda = 0.3) |>
      difference(lag = 4)
  )
aus_production |>
  gg_tsdisplay(
    box_cox(Bricks, lambda = 0.3) |>
      difference(lag = 4) |>
      difference()
  )

# Unit root tests
fb  |> features(Close, unitroot_kpss)
fb  |> features(difference(Close), unitroot_kpss)
fb  |> features(Close, unitroot_ndiffs)

## Seasonal strength
aus_production  |>
  features(
    box_cox(Bricks, lambda = 0.3),
    feat_stl
  )
aus_production  |>
  features(
    box_cox(Bricks, lambda = 0.3),
    unitroot_nsdiffs
  )
aus_production  |>
  features(
    box_cox(Bricks, lambda = 0.3) |>
      difference(lag = 4),
    unitroot_ndiffs
  )

