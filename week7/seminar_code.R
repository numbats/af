library(fpp3)

# Chinese GDP

china <- global_economy |>
  filter(Country == "China")  |>
  transmute(GDP = GDP/1e9)

china |> autoplot(GDP)
china |> autoplot(log(GDP))

china |> model(ETS(GDP)) |> report()
china |> model(ETS(GDP)) |> forecast(h=20) |> autoplot(china, level = NULL)

fit <- china |>
  model(
    ets = ETS(GDP),
    ets_damped = ETS(GDP ~ trend("Ad")),
    ets_bc = ETS(box_cox(GDP, 0.2)),
    ets_log = ETS(log(GDP))
  )

## Check models
fit
glance(fit)
tidy(fit)
augment(fit)
fit  |>
  select(ets)  |>
  gg_tsresiduals()
augment(fit) |> features(.innov, ljung_box, lag = 10)

## Forecasts
fit |>
  forecast(h = "20 years") |>
  autoplot(china, level = NULL) +
  scale_y_log10()


## Add time series cross validation

china_stretch <- china |> stretch_tsibble(.init = 20, .step = 1)

fit <- china_stretch |>
  model(
    ets = ETS(GDP),
    ets_damped = ETS(GDP ~ trend("Ad")),
    ets_bc = ETS(box_cox(GDP, 0.2)),
    ets_log = ETS(log(GDP))
  )
fit
fc <- fit |> forecast(h = 10)
accuracy(fc, china)  |>
  arrange(RMSE)

# Australian gas production

aus_production |>
  autoplot(Gas)

fit <- aus_production |>
  model(
    auto = ETS(Gas),
    hw = ETS(Gas ~ error("M") + trend("A") + season("M")),
    hwdamped = ETS(Gas ~ error("M") + trend("Ad") + season("M")),
  )
fit
fit |> glance()

fit |>
  select(hw) |>
  gg_tsresiduals()

fit |> tidy()

fit |>
  augment() |>
  filter(.model == "hw") |>
  features(.innov, ljung_box, lag = 24)

fit |>
  forecast(h = 36) |>
  filter(.model == "hw") |>
  autoplot(aus_production)

# Try fitting model to data since 1990

fit <- aus_production |>
  filter(year(Quarter) >= 1990) |>
  model(
    auto = ETS(Gas),
    hw = ETS(Gas ~ error("M") + trend("A") + season("M")),
    hwdamped = ETS(Gas ~ error("M") + trend("Ad") + season("M")),
  )
fit
fit |> glance()

fit |>
  select(auto) |>
  gg_tsresiduals()

fit |>
  augment() |>
  filter(.model == "auto") |>
  features(.innov, ljung_box, lag = 24)

fit |>
  forecast(h = 36) |>
  filter(.model == "auto") |>
  autoplot(aus_production)


library(fpp3)

# Victorian tobacco expenditure

vic_tobacco <- aus_tobacco |>
  filter(State == "VIC")
vic_tobacco |>
  autoplot(Expenditure)
fit <- vic_tobacco |>
  model(
    ANN = ETS(Expenditure ~ error("A") + trend("N") + season("N")),
  )
fit |>
  select(ANN) |>
  report()

tidy(fit)
glance(fit)
accuracy(fit)

components(fit) |> autoplot()

fit |>
  select(ANN) |>
  gg_tsresiduals()

fit |>
  augment() |>
  features(.innov, ljung_box, lag = 10)

fc <- fit |>
  forecast(h=5)

fc |>
  filter(.model == "ANN") |>
  autoplot(vic_tobacco) 

# Repeat with test set

fit <- vic_tobacco |>
  filter(year(Quarter) <= 2020) |>
  model(
    ANN = ETS(Expenditure ~ error("A") + trend("N") + season("N")),
    AAN = ETS(Expenditure ~ error("A") + trend("A") + season("N")),
    AAA = ETS(Expenditure ~ error("A") + trend("A") + season("A")),
    naive = NAIVE(Expenditure),
    drift = RW(Expenditure ~ drift())
  )

fc <- fit |>
  forecast(h="3 years")

fc |>
  autoplot(vic_tobacco, level=NULL) 

fc |> accuracy(vic_tobacco) |> arrange(RMSE)

# Repeat with tscv

vic_cig_stretch <- vic_tobacco |>
  stretch_tsibble(.init = 10, .step = 1)

cv_fit <- vic_cig_stretch |>
  model(
    ANN = ETS(Expenditure ~ error("A") + trend("N") + season("N")),
    AAN = ETS(Expenditure ~ error("A") + trend("A") + season("N")),
    AAA = ETS(Expenditure ~ error("A") + trend("A") + season("A")),
    naive = NAIVE(Expenditure),
    drift = RW(Expenditure ~ drift())
  )

cv_fc <- cv_fit |>
  forecast(h = 12) |>
  group_by(.id, .model) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Expenditure", distribution = Expenditure)

cv_fc |>
  accuracy(vic_tobacco, by = c("h", ".model")) |>
  group_by(.model, h) |>
  summarise(RMSSE = sqrt(mean(RMSSE^2))) |>
  ggplot(aes(x=h, y=RMSSE, group=.model, col=.model)) +
  geom_line()

cv_fc |>
  accuracy(vic_tobacco, by = c("h", ".model")) |>
  group_by(.model) |>
  summarise(RMSSE = sqrt(mean(RMSSE^2))) |>
  arrange(RMSSE)
