library(fpp3)

aus_retail <- readr::read_rds("https://bit.ly/monashretaildata")

retail <- aus_retail |>
  summarise(Turnover = sum(Turnover))

retail |>
  autoplot(Turnover)

fit <- retail |>
  model(
    drift = RW(Turnover ~ drift()),
    ets = ETS(Turnover)
  )

fit |>
  forecast(h = "5 years") |>
  autoplot(retail)

fit |>
  augment()
fit |>
  tidy()
fit |>
  glance()

fit |>
  augment() |>
  autoplot(.fitted) +
  geom_line(aes(y = Turnover), colour = "steelblue")


fit |>
  augment() |>
  autoplot(.innov)
fit |>
  augment() |>
  gg_season(.innov)

fit |>
  select(ets) |>
  gg_tsresiduals()

fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

# H0: No significant autocorrelations in the residuals -- WN
# H1: Significant autocorrelations (ACFs) -- NOT WN

fit |>
  select(ets) |>
  augment()

fit |>
  augment() |>
  as_tibble() |>
  group_by(.model) |>
  summarise(mean(.resid, na.rm=TRUE))

fit |>
  accuracy()


fit |>
  forecast(h = "5 years") |>
  autoplot(retail)



fit_train <- retail |>

  # Hide some data (test set)
  filter(year(Month) < 2010) |>

  model(
    drift = RW(Turnover ~ drift()),
    ets = ETS(Turnover)
  )

fit_train |>
  forecast(h = "5 years") |>
  autoplot(retail)

fit_train |>
  forecast(h = "5 years") |>
  accuracy(retail)





fit_cv <- retail |>

  # Fold the data (cross-validation)
  stretch_tsibble(.init = 6 * 12, .step = 2 * 12) |>

  # Hide some data (test set)
  # filter(year(Month) < 2010) |>

  model(
    drift = RW(Turnover ~ drift()),
    ets = ETS(Turnover)
  )

fit_cv |>
  forecast(h = "2 years") |>
  accuracy(retail)
