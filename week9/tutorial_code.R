library(fpp3)
usa_economy <- global_economy |>
  filter(Code == "USA")
usa_economy |>
  autoplot(GDP)
usa_economy |>
  gg_tsdisplay(GDP, plot_type = "partial")

# Step 1: Transform (box_cox / log)
usa_economy |>
  gg_tsdisplay(
    box_cox(GDP, 1/3),
    plot_type = "partial"
  )
usa_economy |>
  features(
    GDP,
    guerrero
  )

# Step 2: Seasonally difference IF seasonal
# Skip - data is non-seasonal

# Step 3: Regular (first-order) difference IF non-stationary
usa_economy |>
  features(
    box_cox(GDP, 1/3),
    unitroot_kpss
  )
# H0: stationary, H1: non-stationary
# p < 0.05, therefore REJECT H0, and data is non-stationary

usa_economy |>
  gg_tsdisplay(
    difference(box_cox(GDP, 1/3), lag = 1),
    plot_type = "partial"
  )
usa_economy |>
  features(
    difference(box_cox(GDP, 1/3), lag = 1),
    unitroot_kpss
  )
# p > 0.05 (p = 0.1), therefore we FAIL to reject H0..
# data is stationary

# From the PACF, we find AR(p)
# p = last significant lag from PACF
# q = 0

# From the ACF, we find MA(q)
# q = last significant lag from ACF
# p = 0

usa_economy |>
  model(
    # From PACF: ARIMA(p = 1, d = 1, q = 0) + c
    ar = ARIMA(box_cox(GDP, 1/3) ~ 1 + pdq(p = 1, d = 1, q = 0)),
    # From ACF: ARIMA(p = 0, d = 1, q = 1) + c
    ma = ARIMA(box_cox(GDP, 1/3) ~ 1 + pdq(p = 0, d = 1, q = 1))
  ) |>
  glance()

nz_to_aus <- aus_arrivals |>
  filter(Origin == "NZ")

nz_to_aus |>
  gg_tsdisplay(
    Arrivals,
    plot_type = "partial"
  )

# Step 1: Transform
nz_to_aus |>
  gg_tsdisplay(
    sqrt(Arrivals),
    plot_type = "partial"
  )

# Step 2: Seasonally difference IF seasonal
nz_to_aus |>
  gg_tsdisplay(
    difference(sqrt(Arrivals), lag = 4),
    plot_type = "partial"
  )

# Step 3: Regular difference IF non-stationary
nz_to_aus |>
  features(
    difference(sqrt(Arrivals), lag = 4),
    list(unitroot_kpss, ~ mean(., na.rm= TRUE))
  )
# Data is stationary, no need to regular difference

# from PACF
# ARIMA(p = 1, d = 0, q = 0)(P, D = 1, Q)[4] + c

# from ACF
# ARIMA(p = 0, d = 0, q = 3)(P, D = 1, Q)[4] + c
