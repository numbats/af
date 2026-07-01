library(fpp3)

# Chinese GDP

chinese_gdp <- global_economy |>
  filter(Country == "China") |>
  mutate(GDP_pc = GDP / Population)

chinese_gdp |> autoplot(GDP_pc)

fit <- chinese_gdp |>
  model(
    #AAN0 = ETS(GDP_pc ~ error("A") + trend("A", alpha = 0.3, beta = 0.3)),
    #ANNalpha0 = ETS(GDP_pc ~ error("A") + trend("N", alpha = 0.0002)),
    #ANNalpha1 = ETS(GDP_pc ~ error("A") + trend("N", alpha = 0.9998)),
    #AANalpha0 = ETS(GDP_pc ~ error("A") + trend("A", alpha = 0.0002)),
    #AANalpha1 = ETS(GDP_pc ~ error("A") + trend("A", alpha = 0.9998)),
    #beta0 =  ETS(GDP_pc ~ error("A") + trend("A", beta = 0)),
    #beta_gt_alpha =  ETS(GDP_pc ~ error("A") +
    #                       trend("A", alpha = 0.3, beta = 0.31)),
    #AAN = ETS(GDP_pc ~ error("A") + trend("A")),
    #AAdN = ETS(GDP_pc ~ error("A") + trend("Ad")),
    #ANN = ETS(GDP_pc ~ error("A") + trend("N")),
    #MAN = ETS(GDP_pc ~ error("M") + trend("A")),
    #MAdN = ETS(GDP_pc ~ error("M") + trend("Ad")),
    #MNN = ETS(GDP_pc ~ error("M") + trend("N")),
    #BC = ETS(box_cox(GDP_pc, lambda = 0.3) ~ error("A") + trend("A")),
    ets1 = ETS(GDP_pc ~ error("A")),
    ets2 = ETS(GDP_pc)
  )

fit

fit |>
  select(AAN) |>
  report()

fc <- fit |> forecast(h = 10)

fc |>
  #filter(.model == "AAN") |>
  autoplot(chinese_gdp, level = NULL)

fc |>
  filter(.model %in% c("BC", "MAN")) |>
  autoplot(chinese_gdp)


# Prediction intervals

fit <- chinese_gdp |>
  model(ets = ETS(GDP_pc ~ error("A") + trend("N", alpha = 0.5)))

fit |>
  forecast(h = 5) |>
  mutate(pi = hilo(GDP_pc, level = 95))

ellT <- components(fit) |> tail(1) |> pull(level)
sigma2 <- glance(fit) |> pull(sigma2)

ellT - qnorm(0.975) * sqrt(sigma2) * sqrt(1 + (0:4) / 4)
ellT + qnorm(0.975) * sqrt(sigma2) * sqrt(1 + (0:4) / 4)
