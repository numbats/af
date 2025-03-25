library(fpp3)

## HOLIDAYS

holidays <- tourism |>
  as_tibble() |>
  filter(Purpose == "Holiday") |>
  summarise(Trips = sum(Trips), .by = c("State", "Quarter")) |>
  as_tsibble(index = Quarter, key = State)

# Forecasting with a transformation

fc <- holidays |>
  model(TSLM(sqrt(Trips) ~ trend() + season())) |>
  forecast(h = "5 years")

fc |>
  filter(State == "Western Australia") |>
  autoplot(holidays)

# Forecasting with a decomposition

my_dcmp_spec <- decomposition_model(
  STL(Trips),
  MEAN(season_adjust), # Model for seasonally adjusted series
  SNAIVE(season_year) # Model for seasonal component
)

fit <- holidays |>
  model(stl_naive = my_dcmp_spec)

fc <- fit |>
  forecast(h = "5 years")

fc |> filter(State == "Queensland") |> autoplot(holidays)

## BRICK production

bricks <- aus_production |>
  select(Bricks) |>
  filter(!is.na(Bricks))
bricks |> autoplot(Bricks)
fit <- bricks |>
  model(stl = STL(sqrt(Bricks) ~ season(window = 13)))
fit |>
  components() |>
  autoplot()
fit |>
  components() |>
  as_tsibble() |>
  autoplot(season_adjust)

mdl <- decomposition_model(
  STL(sqrt(Bricks) ~ season(window = 13), robust = TRUE),
  NAIVE(season_adjust)
)
fit <- bricks |> model(mdl)
fc <- fit |>
  forecast(h = 24)
fc |> autoplot(bricks)

## AFGHANISTAN population

global_economy |>
  filter(Country == "Afghanistan") |>
  autoplot(Population)

fit <- global_economy |>
  filter(Country == "Afghanistan") |>
  model(
    linear = TSLM(Population ~ trend()),
    piecewise = TSLM(Population ~ trend(knots = c(1980, 1989)))
  )
fit |>
  select(piecewise) |>
  report()

fc <- fit |>
  forecast(h = 5)

fc |>
  autoplot(global_economy)


## OLYMPIC RUNNING

olympic_running |>
  as_tibble() |>
  ggplot(aes(x = Year, y = Time, colour = Sex)) +
  geom_line() +
  facet_wrap(~Length, scales = "free_y")

fit <- olympic_running |>
  model(
    linear = TSLM(Time ~ trend()),
  )

tidy(fit) |>
  filter(term == "trend()") |>
  mutate(change_per_year = estimate / 4) |>
  select(Length, Sex, change_per_year)

fit |>
  forecast(h = 3) |>
  as_tibble() |>
  filter(Year == 2028) |>
  select(Length, Sex, .mean)

# These are all world records, and are very unlikely

# Women have not been competing for as long as men and need
# different models
male_fc <- olympic_running |>
  filter(Sex == "men") |>
  model(
    pwlinear = TSLM(Time ~ trend(knots = c(1920, 1990))),
  ) |>
  forecast(h = 3)
female_fc <- olympic_running |>
  filter(Sex == "women") |>
  model(
    pwlinear = TSLM(Time ~ trend(knots = c(1980))),
  ) |>
  forecast(h = 3)
fc <- bind_rows(male_fc, female_fc)
fc |>
  as_tibble() |>
  ggplot(aes(x = Year, y = .mean, colour = Sex)) +
  geom_line() +
  geom_line(data = olympic_running, aes(y = Time), linetype = "dashed") +
  facet_wrap(~Length, scales = "free_y")

# Warnings from cases where no data available before a knot
# In most cases, these forecasts look much better, although the upward trend
# in some cases is unrealistic. A better model would be either downward trending
# or flat in the forecast period.
