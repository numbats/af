library(fpp3)

# Weekly data with Fourier terms

us_gasoline |>
  autoplot(Barrels) +
  labs(y = "Weekly US finished motor gasoline product supplied (million barrels)")

fit <- us_gasoline |>
  model(
    K01 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 1)),
    K02 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 2)),
    K03 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 3)),
    K04 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 4)),
    K05 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 5)),
    K06 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 6)),
    K07 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 7)),
    K08 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 8)),
    K09 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 9)),
    K10 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 10)),
    K11 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 11)),
    K12 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 12)),
    K13 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 13)),
    K14 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 14)),
    K15 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 15)),
    K16 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 16)),
    K20 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 20)),
    K26 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 26))
  )
glance(fit) |>
  select(.model, r_squared, adj_r_squared, df, AICc, CV) |>
  arrange(CV)

augment(fit) |>
  filter(.model %in% c("K06", "K01", "K26")) |>
  ggplot(aes(x = Week, y = Barrels)) +
  geom_line(colour = "gray") +
  geom_line(aes(y = .fitted, col = .model), linewidth = 1) +
  facet_grid(.model ~ .)

fit |>
  select(K06) |>
  forecast(h = "2 years") |>
  autoplot(us_gasoline)

fit |>
  select(K06) |>
  gg_tsresiduals()

# US consumption quarterly changes

us_change |>
  pivot_longer(-Quarter, names_to = "Measure", values_to = "Change") |>
  ggplot(aes(x = Quarter, y = Change)) +
  geom_line() +
  facet_grid(Measure ~ ., scales = "free_y")

us_change |>
  GGally::ggpairs(columns = c(3:6, 2))

fit_all <- us_change |>
  model(
    TSLM(Consumption ~ Income + Production + Unemployment + Savings),
    TSLM(Consumption ~ Production + Unemployment + Savings),
    TSLM(Consumption ~ Income + Unemployment + Savings),
    TSLM(Consumption ~ Income + Production + Savings),
    TSLM(Consumption ~ Income + Production + Unemployment),
    TSLM(Consumption ~ Income + Production),
    TSLM(Consumption ~ Income + Unemployment),
    TSLM(Consumption ~ Income + Savings),
    TSLM(Consumption ~ Production + Unemployment),
    TSLM(Consumption ~ Production + Savings),
    TSLM(Consumption ~ Unemployment + Savings),
    TSLM(Consumption ~ Income),
    TSLM(Consumption ~ Production),
    TSLM(Consumption ~ Unemployment),
    TSLM(Consumption ~ Savings),
    TSLM(Consumption ~ 1),
  )

glance(fit_all) |>
  select(.model, adj_r_squared, AICc, BIC, CV) |>
  arrange(CV)

fit_consBest <- fit_all |> select(1)

report(fit_consBest)

augment(fit_consBest) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(
    y = NULL,
    title = "Percent change in US consumption expenditure"
  ) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = NULL))

augment(fit_consBest) |>
  ggplot(aes(y = .fitted, x = Consumption)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percentage change in US consumption expenditure"
  ) +
  geom_abline(intercept = 0, slope = 1)

fit_consBest |> gg_tsresiduals()

augment(fit_consBest) |>
  left_join(us_change) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point()

augment(fit_consBest) |>
  left_join(us_change) |>
  ggplot(aes(x = Income, y = .resid)) +
  geom_point()

fit_consBest |> forecast(
  new_data(us_change, 4) |>
    mutate(Income = c(0, 1, 3, 0), Savings = 0, Unemployment = 0, Production = 0)
)


future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) |>
    mutate(Income = 2, Savings = 0.5, Unemployment = 0, Production = 0),
  Decrease = new_data(us_change, 4) |>
    mutate(Income = -1, Savings = -0.5, Unemployment = 0, Production = 0),
  names_to = "Scenario"
)

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change |> autoplot(Consumption) +
  labs(y = "% change in US consumption") +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")
