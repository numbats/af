library(fpp3)
aus_economy <- global_economy |>
  filter(Code == "AUS")

aus_economy |>
  autoplot(Population)

aus_economy |>
  model(
    TSLM(Population ~ trend()),
    # MEAN(Population)
    RW(Population ~ drift())
  ) |>
  forecast(h = "10 years") |>
  autoplot(aus_economy)

# lm()
sim <- tsibble(
  time = 1:100,
  y = rnorm(100),
  index = time
)
sim |>
  autoplot(y)
sim |>
  slice(-n()) |>
  model(
    MEAN(y),
    NAIVE(y),
    RW(y ~ drift()),
    SNAIVE(y ~ lag(4))
  ) |>
  forecast(h = 50) |>
  autoplot(sim, level = NULL)

aus_production |>
  autoplot(Bricks)
aus_production |>
  filter(!is.na(Bricks)) |>
  model(
    # NAIVE(Bricks),
    # MEAN(Bricks),
    # SNAIVE(Bricks),
    # RW(Bricks ~ drift())
    SNAIVE(Bricks ~ drift())
  ) |>
  forecast(h = "5 years") |>
  # autoplot(aus_production, level = NULL)
  # autoplot(aus_production, level = 50, alpha = 0.3)
  autoplot(aus_production) +
  facet_wrap(vars(.model)) +
  guides(colour = "none", fill = "none") +
  theme(legend.position = "bottom")


aus_total <- aus_retail |>
  summarise(Turnover = sum(Turnover))

aus_total |>
  autoplot(Turnover)

aus_total |>
  model(
    # SNAIVE(Turnover),
    # RW(Turnover ~ drift()),
    # SNAIVE(Turnover ~ drift()),
    TSLM(box_cox(Turnover, 0.5) ~ trend() + season())
  ) |>
  forecast(h = "25 years") |>
  autoplot(aus_total)
