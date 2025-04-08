library(fpp3)

# Chinese GDP

chinese_gdp <- global_economy |>
  filter(Country == "China") |>
  mutate(GDP_pc = GDP/Population)

chinese_gdp |> autoplot(GDP_pc)

fit <- chinese_gdp |>
  model(
    ets = ETS(GDP_pc ~ error("A") + trend("A", alpha = 0.3, beta = 0.3))
  )

report(fit)

fc <- fit |> forecast(h=10)

fc |> autoplot(chinese_gdp)


# Prediction intervals

fit <- chinese_gdp |>
  model(ets = ETS(GDP_pc ~ error("A") + trend("N", alpha = 0.5)))

fit |>
  forecast(h = 5) |>
  mutate(pi = hilo(GDP_pc, level = 95))

ellT <- components(fit) |> tail(1) |> pull(level)
sigma2 <- glance(fit) |> pull(sigma2)

ellT - qnorm(0.975) * sqrt(sigma2) * sqrt(1 + (0:4)/4)
ellT + qnorm(0.975) * sqrt(sigma2) * sqrt(1 + (0:4)/4)

