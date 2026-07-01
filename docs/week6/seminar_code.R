library(fpp3)

# Australian fertility

aus_ave_fert <- aus_fertility |> filter(Region == "Australia", Age <= 30) |> summarise(Rate = mean(Rate))

aus_ave_fert |> autoplot(Rate)

fit <- aus_ave_fert |>
  model(
    ANN = ETS(Rate ~ error("A") + trend("N") + season("N")),
    AAN = ETS(Rate ~ error("A") + trend("A")),
    AAdN = ETS(Rate ~ error("A") + trend("Ad")),
    MNN = ETS(Rate ~ error("M") + trend("N") + season("N")),
    MAN = ETS(Rate ~ error("M") + trend("A")),
    MAdN = ETS(Rate ~ error("A") + trend("Ad")),
  )
fit |>
  select(MAdN) |>
  report()

tidy(fit)
glance(fit)
accuracy(fit)

components(fit) |> autoplot()

fit |>
  select(AAN) |>
  gg_tsresiduals()

fit |>
  augment() |>
  features(.innov, ljung_box, lag = 10)

fc <- fit |>
  forecast(h=50)

fc |>
  filter(.model == "MAdN") |>
  autoplot(aus_ave_fert)

# Repeat with test set

fit <- aus_ave_fert |>
  filter(Year <= 2018) |>
  model(
    ANN = ETS(Rate ~ error("A") + trend("N") + season("N")),
    AAN = ETS(Rate ~ error("A") + trend("A") + season("N")),
    MNN = ETS(Rate ~ error("A") + trend("N") + season("N")),
    MAN = ETS(Rate ~ error("A") + trend("A") + season("N")),
    AAdN = ETS(Rate ~ error("A") + trend("Ad") + season("N")),
    MAdN = ETS(Rate ~ error("A") + trend("Ad") + season("N")),
    naive = NAIVE(Rate),
    drift = RW(Rate ~ drift())
  )

tidy(fit)
glance(fit)


fc <- fit |>
  forecast(h="4 years")

fc |>
  autoplot(aus_ave_fert, level=NULL)

fc |> accuracy(aus_ave_fert) |> arrange(RMSE)

# Repeat with tscv

fert_stretch <- aus_ave_fert |>
  stretch_tsibble(.init = 10, .step = 1)

cv_fit <- fert_stretch |>
  model(
    ANN = ETS(Rate ~ error("A") + trend("N") + season("N")),
    AAN = ETS(Rate ~ error("A") + trend("A") + season("N")),
    MNN = ETS(Rate ~ error("M") + trend("N") + season("N")),
    MAN = ETS(Rate ~ error("M") + trend("A") + season("N")),
    AAdN = ETS(Rate ~ error("A") + trend("Ad") + season("N")),
    MAdN = ETS(Rate ~ error("M") + trend("Ad") + season("N")),
    naive = NAIVE(Rate),
    drift = RW(Rate ~ drift())
  )

cv_fc <- cv_fit |>
  forecast(h = 12) |>
  group_by(.id, .model) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Rate", distribution = Rate)

cv_fc |>
  accuracy(aus_ave_fert, by = c("h", ".model")) |>
  group_by(.model, h) |>
  summarise(RMSE = sqrt(mean(RMSE^2))) |>
  ggplot(aes(x=h, y=RMSE, group=.model, col=.model)) +
  geom_line()

cv_fc |>
  accuracy(aus_ave_fert, by = c("h", ".model")) |>
  group_by(.model) |>
  summarise(RMSE = sqrt(mean(RMSE^2))) |>
  arrange(RMSE)
