library(fpp3)

amzn_stock <- gafa_stock |>
  filter(Symbol == "AMZN")

amzn_stock |>
  autoplot(Close)

# Three steps to making time series stationary.

# Step 1: Transform your data IF multiplicative
# log(), box_cox(), ...
amzn_stock |>
  autoplot(log(Close))

# Step 2: Seasonally difference (D > 0) IF seasonal
# difference(y, lag = m)

# SKIP - not seasonal

# Step 3: Regular difference (d > 0) IF non-stationary
# difference(y)
amzn_stock |>
  autoplot(difference(log(Close)))
amzn_stock |>
  gg_tsdisplay(
    difference(log(Close)),
    plot_type = "partial"
  )



pelt |> autoplot(Lynx)



# Three steps to making time series stationary.
souvenirs |>
  gg_tsdisplay(
    Sales,
    plot_type = "partial"
  )

# Step 1: Transform your data IF multiplicative
# log(), box_cox(), ...
souvenirs |>
  gg_tsdisplay(
    log(Sales),
    plot_type = "partial"
  )


# Step 2: Seasonally difference (D > 0) IF seasonal
# difference(y, lag = m)

souvenirs |>
  gg_tsdisplay(
    difference(log(Sales), lag = 12),
    plot_type = "partial"
  )

# Step 3: Regular difference (d > 0) IF non-stationary
# SKIP - stationary

souvenirs |>
  features(
    difference(log(Sales), lag = 12),
    list(unitroot_kpss, ljung_box), lag = 24
  )
# H0: Stationary
# H1: Non-stationary

souvenirs |>
  model(ARIMA(log(Sales)))
