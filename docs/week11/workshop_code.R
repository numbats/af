library(fpp3)

# Create daily data set

vic_daily <- vic_elec |>
  mutate(
    Day = factor(wday(Date, label = TRUE), ordered = FALSE),
    Weekend = Day %in% c("Sat", "Sun"),
    Working_Day = !Weekend & !Holiday
  ) |>
  index_by(Date = as_date(Time)) |>
  summarise(
    Demand = sum(Demand)/1e3,
    Temperature = max(Temperature),
    Day = first(Day),
    Working_Day = first(Working_Day)
  )

vic_daily |>
  ggplot(aes(x = Temperature, y = Demand, color = Working_Day)) +
  geom_point()


fit <- vic_daily |>
  model(
    lm1 = TSLM(Demand ~ Temperature + I(Temperature^2) + Working_Day),
    lm2 = TSLM(Demand ~ Temperature + I(Temperature^2) + Working_Day +
                 fourier(period = 365.25, K=3)),
    lm3 = TSLM(Demand ~ Temperature + I(Temperature^2) + Working_Day +
                 fourier(period = 365.25, K=30)),
    lm4 = TSLM(Demand ~ Temperature + I(Temperature^2) +
                 Working_Day + Day +
                 fourier(period = 365.25, K=30)),
    dynreg = ARIMA(Demand ~ Temperature + I(Temperature^2) +
                 Working_Day + Day +
                 fourier(period = 365.25, K=30)),

  )
glance(fit) |> select(.model, AICc, CV, df)
fit |> select(dynreg) |> report()

augment(fit) |>
  filter(.model == "lm4") |>
  left_join(vic_daily) |>
  ggplot(aes(x = Temperature, y = .innov)) +
  geom_point()

fit |> select(lm4) |> gg_tsresiduals()

augment(fit) |> filter(.model == "lm4") |>
  gg_tsdisplay(.innov, plot_type = "partial")

augment(fit) |>
  filter(.model == "lm4") |>
  model(ARIMA(.innov ~ pdq(d=0) + PDQ(D=0)))


fit |>
  select(lm4) |>
  forecast(y = "2 weeks")

nd <- new_data(vic_daily, 14) |>
  mutate(
    Day = factor(wday(Date, label = TRUE), ordered = FALSE),
    Weekend = Day %in% c("Sat", "Sun"),
    Holiday = Date == as.Date("2025-01-01"),
    Working_Day = !Weekend & !Holiday,
    Temperature = 25
  )
fit |>
  forecast(new_data = nd) |>
  filter(.model == "dynreg") |>
  autoplot(vic_daily |> dplyr::filter(Date >= "2014-11-01"))

