library(fpp3)

# Victorian tobacco expenditure

vic_tobacco <- aus_tobacco |>
  filter(State == "VIC")
vic_tobacco |>
  autoplot(Expenditure)
fit <- vic_tobacco |>
  model(
    #AAN = ETS(Expenditure ~ error("A") + trend("A") + season("N")),
    #MAM = ETS(Expenditure ~ error("M") + trend("A") + season("M")),
    ets = ETS(Expenditure)
  )
fit |>
  select(MAM) |>
  report()

tidy(fit)
glance(fit)
accuracy(fit)

components(fit) |> autoplot()

fit |>
  select(MAM) |>
  gg_tsresiduals()

fit |>
  augment() |>
  features(.innov, ljung_box, lag = 8)

fc <- fit |>
  forecast(h="5 years")

fc |>
  autoplot(vic_tobacco)

# Repeat with test set

fit <- vic_tobacco |>
  filter(year(Quarter) <= 2020) |>
  model(
    AAN = ETS(Expenditure ~ error("A") + trend("A") + season("N")),
    MAM = ETS(Expenditure ~ error("M") + trend("A") + season("M")),
    ets = ETS(Expenditure),
    snaive_drift = SNAIVE(log(Expenditure) ~ drift())
  )

fit

fc <- fit |>
  forecast(h="5 years")

fc |>
  autoplot(vic_tobacco, level=NULL)

fc |> accuracy(vic_tobacco) |> arrange(RMSE)

# Repeat with tscv

vic_cig_stretch <- vic_tobacco |>
  stretch_tsibble(.init = 12, .step = 1)

cv_fit <- vic_cig_stretch |>
  model(
    AAN = ETS(Expenditure ~ error("A") + trend("A") + season("N")),
    MAM = ETS(Expenditure ~ error("M") + trend("A") + season("M")),
    ets = ETS(Expenditure),
    snaive_drift = SNAIVE(log(Expenditure) ~ drift())
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


## Automatic modelling of PBS data

fit_PBS <- PBS |>
  model(ets = ETS(Scripts))

# Which models are chosen?
fit_PBS |>
  mutate(model_name = as.character(ets)) |>
  count(model_name) |>
  arrange(desc(n))

# Best and  Worst fitting model
acc_PBS <- accuracy(fit_PBS)

perfect_fits <- acc_PBS |>
  filter(RMSE == 0) |>
  left_join(PBS) |>
  as_tsibble(index = Month, key = c(Concession, Type, ATC1, ATC2))
perfect_fits |> autoplot(Scripts)

best_fit <- acc_PBS |>
  filter(RMSSE == min(RMSSE, na.rm = TRUE)) |>
  left_join(PBS) |>
  as_tsibble(index = Month, key = c(Concession, Type, ATC1, ATC2))
best_fit |> autoplot(Scripts)
fit <- best_fit |> model(ETS(Scripts))
report(fit)
best_fit |> autoplot(Scripts) + autolayer(fitted(fit), col = "red")
fit |> forecast(h = 36) |> autoplot(PBS, level = NULL)

worst_fit <- acc_PBS |>
  filter(RMSSE == max(RMSSE, na.rm = TRUE)) |>
  left_join(PBS) |>
  as_tsibble(index = Month, key = c(Concession, Type, ATC1, ATC2))
worst_fit |> autoplot(Scripts)
fit <- worst_fit |> model(ETS(Scripts))
report(fit)
worst_fit |> autoplot(Scripts) + autolayer(fitted(fit), col = "red")
fit |> forecast(h = 36) |> autoplot(PBS, level = NULL)
