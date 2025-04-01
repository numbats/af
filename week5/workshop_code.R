library(fpp3)

## hh_budget exercise

hh_budget |>
  autoplot(Wealth)

# 1. Create training set by withholding last four years
train <- hh_budget |>
  filter(Year <= max(Year) - 4)
#2. Fit benchmarks
fit <- train |>
  model(
    # MEAN method
    MEAN(Wealth),

    # RW w/ drift method
    RW(Wealth ~ drift()),

    # NAIVE method
    NAIVE(Wealth)
  )
fc <- fit |>
  forecast(h = "4 years")

fc |>
  autoplot(hh_budget)


# 3. Compute accuracy
fc |>
  accuracy(hh_budget) |>
  arrange(Country, RMSE)

fc |>
  accuracy(hh_budget) |>
  filter(Country == "Australia")

fc |>
  accuracy(hh_budget) |>
  group_by(.model) |>
  summarise(mean(MASE))

augment(fit) |>
  filter(.model == "RW(Wealth ~ drift())") |>
  autoplot(.fitted) +
  geom_line(aes(y = Wealth)) +
  facet_wrap(vars(Country), scales = "free_y")

# 4. Do the residuals resemble white noise?
# e_t iid~ N(0, sigma^2)?

fit |>
  filter(Country == "Australia") |>
  select(drift) |>
  gg_tsresiduals()
fit |>
  filter(Country == "Canada") |>
  select(drift) |>
  gg_tsresiduals()
fit |>
  filter(Country == "Japan") |>
  select(drift) |>
  gg_tsresiduals()
fit |>
  filter(Country == "USA") |>
  select(drift) |>
  gg_tsresiduals()
