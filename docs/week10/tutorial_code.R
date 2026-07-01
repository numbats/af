library(fpp3)

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

pelt |>
  autoplot(Lynx)

# Data is stationary, no need to regular difference

# from PACF
# ARIMA(p = 1, d = 0, q = 0)(P, D = 1, Q)[4] + c

# from ACF
# ARIMA(p = 0, d = 0, q = 3)(P, D = 1, Q)[4] + c

# (AR)(SAR)(I)(SI)y*_t = c + (1 + \theta_1 B + \theta_2 B^2 + \theta_3 B^3)(SMA)e_t





# Non-seasonal: LAST significant lag (up to m-1)

## ACF: MA(q = 3)
## PACF: AR(p = 1)

# Seasonal: LAST significant SEASONAL lag

## ACF: SMA(Q = 0)
## PACF: SAR(P = 2)


# YOU CAN: Mix and match
# MA + SMA: ARIMA(p = 0,d = 0,q = 3)(P = 0,D = 1,Q = 0)[m = 4] + c
# AR + SAR: ARIMA(1, 0, 0)(2, 1, 0)[4] + c
# MA + SAR: ARIMA(0, 0, 3)(2, 1, 0)[4] + c
# AR + SMA: ARIMA(1, 0, 0)(0, 1, 0)[4] + c

# YOU CANNOT DO
# MA + AR
# SMA + SAR

# Estimating the ARIMA models

fit <- nz_aus |>
  model(
    # auto (force the same differences if you have ignored the tests)
    auto = ARIMA(sqrt(Arrivals) ~ pdq(d = 0) + PDQ(D = 1)),

    # ARIMA(p = 0,d = 0,q = 3)(P = 0,D = 1,Q = 0)[m = 4] + c
    ARIMA(sqrt(Arrivals) ~ 1 + pdq(0,0,3) + PDQ(0,1,0)),

    # ARIMA(1, 0, 0)(2, 1, 0)[4] + c
    ARIMA(sqrt(Arrivals) ~ 1 + pdq(1,0,0) + PDQ(2,1,0)),

    # ARIMA(0, 0, 3)(2, 1, 0)[4] + c
    ARIMA(sqrt(Arrivals) ~ 1 + pdq(0,0,3) + PDQ(2,1,0)),

    # ARIMA(1, 0, 0)(0, 1, 0)[4] + c
    ARIMA(sqrt(Arrivals) ~ 1 + pdq(1,0,0) + PDQ(0,1,0)),
  )
glance(fit)
fit

best_fit <- fit |>
  select(Origin, auto)
best_fit |>
  forecast(h = "10 years") |>
  autoplot(nz_aus)

best_fit |>
  gg_tsresiduals()

augment(best_fit) |>
  features(.innov, ljung_box, lag = 8,
           dof = 5) # p + q + P + Q
