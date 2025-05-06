library(fpp3)

# Study effect of c and d

global_economy |>
  filter(Country == "Australia") |>
  #model(ARIMA(Imports ~ 0 + pdq(d = 0))) |>
  #model(ARIMA(Imports ~ 1 + pdq(d = 0))) |>
  #model(ARIMA(Imports ~ 0 + pdq(d = 1))) |>
  #model(ARIMA(Imports ~ 1 + pdq(d = 1))) |>
  #model(ARIMA(Imports ~ 0 + pdq(d = 2))) |>
  model(ARIMA(Imports ~ 1 + pdq(d = 2), order_constraint = TRUE)) |>
  forecast(h = 80) |>
  autoplot(global_economy, level = NULL)

# Simulate an AR(1)

y <- numeric(100)
e <- rnorm(100)
phi <- 1
for(i in 2:100)
  y[i] <- phi*y[i-1] + e[i]
sim <- tsibble(t = seq_len(100), y = y, index = t)
sim |>
  gg_tsdisplay(y, plot_type = "partial") +
  labs(title = "Simulated AR(1) process", y = "y")


# Simulate an MA(1)

y <- numeric(100)
e <- rnorm(100)
theta <- Inf
for(i in 2:100)
  y[i] <- e[i] + theta*e[i-1]
sim <- tsibble(t = seq_len(100), y = y, index = t)
sim |>
  autoplot()

sim |>
  gg_tsdisplay(y, plot_type = "partial") +
  labs(title = "Simulated MA(1) process", y = "y")



# Simulate an AR(2)

y <- numeric(100)
e <- rnorm(100)
phi1 <- 1.35
#phi2 <- -0.75
#phi2 <- -0.45
phi2 <- -0.25
for(i in 3:100)
  y[i] <- phi1*y[i-1] + phi2 * y[i-2] + e[i]
sim <- tsibble(t = seq_len(100), y = y, index = t)
sim |>
  gg_tsdisplay(y, plot_type = "partial") +
  labs(title = "Simulated AR(2) process", y = "y")



