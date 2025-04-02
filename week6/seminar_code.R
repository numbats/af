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
