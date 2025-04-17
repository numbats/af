library(fpp3)

aus_production |>
  autoplot(Gas)

# ETS

# error: M (A/M)
# trend: A/Ad (N/A/Ad/M/Md)
# season: M (N/A/M)

fit <- aus_production |>
  # filter(Quarter >= yearquarter("1990 Q1")) |>
  model(
    ZZZ = ETS(Gas),
    AZZ = ETS(Gas ~ error("A")),
    madm = ETS(Gas ~ error("M") + trend("Ad") + season("M")),
    aaa = ETS(Gas ~ error("A") + trend("A") + season("A")),
    log_aaa = ETS(log(Gas) ~ error("A") + trend("A") + season("A")),
    log_mam = ETS(log(Gas) ~ error("M") + trend("A") + season("M"))
  )


fit |>
  report()

fit |>
  select(ZZZ) |>
  forecast(h = "5 years") |>
  autoplot(aus_production |> tail(12))

fit |>
  select(ZZZ) |>
  gg_tsresiduals()

fit |>
  glance()



fit <- aus_production |>
  stretch_tsibble(
    .init = 4*4, .step = 1*4
  ) |>
  # filter(Quarter >= yearquarter("1990 Q1")) |>
  model(
    ZZZ = ETS(Gas ~ error("M") + trend("A") + season("M")),
    AZZ = ETS(Gas ~ error("A")),
    madm = ETS(Gas ~ error("M") + trend("Ad") + season("M")),
    aaa = ETS(Gas ~ error("A") + trend("A") + season("A")),
    log_aaa = ETS(log(Gas) ~ error("A") + trend("A") + season("A")),
    log_mam = ETS(log(Gas) ~ error("M") + trend("A") + season("M"))
  )
accuracy(fit)
fit


fc <- fit |>
  forecast(h = "5 years")

fc |>
  accuracy(aus_production)
