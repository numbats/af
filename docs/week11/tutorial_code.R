library(fpp3)
get_my_data <- function(student_id) {
  set.seed(student_id)
  all_data <- readr::read_rds("https://bit.ly/monashretaildata")
  while(TRUE) {
    retail <- filter(all_data, `Series ID` == sample(`Series ID`, 1))
    if(!any(is.na(fill_gaps(retail)$Turnover))) return(retail)
  }
}
# Replace the argument with your student ID
retail <- get_my_data(12345678)
retail


library(readabs)
abs_data <- read_abs(cat_no = "8501.0", tables = 11)

# abs_data |> distinct(series_id)
abs_data |>
  filter(series_id == "A3349335T")


souvenirs |>
  autoplot(log(Sales))

fit <- souvenirs |>
  model(
    TSLM(log(Sales) ~ trend() + season() + festival)
  )

souvenirs |>
  autoplot(Sales, alpha = 0.3) +
  autolayer(
    augment(fit), .fitted,
    colour = "hotpink", linewidth = 1
  )

us_gasoline |>
  autoplot(Barrels)

# Trend







fit <- us_gasoline |>
  model(
    TSLM(Barrels ~ trend(knots = yearweek(c("2007 W1", "2013 W1"))))
  )
us_gasoline |>
  autoplot(Barrels, alpha = 0.3) +
  autolayer(
    augment(fit), .fitted,
    colour = "hotpink", linewidth = 1
  ) +
  scale_x_yearmonth(date_breaks = "3 year")
tidy(fit)


# The trend between 2007 and 2013 is...
0.00276 + (-0.00510)


# Seasonality
fit <- us_gasoline |>
  model(
    harmonic = TSLM(Barrels ~ trend(knots = yearweek(c("2007 W1", "2013 W1"))) + fourier(period = 52.178, K = 9))
  )
us_gasoline |>
  autoplot(Barrels, alpha = 0.3) +
  autolayer(
    augment(fit), .fitted,
    colour = "hotpink", linewidth = 1
  ) +
  scale_x_yearmonth(date_breaks = "3 year")
tidy(fit) |> print(n=Inf)

glance(fit)

365.25/7


us_gasoline |>
  model(STL(Barrels)) |>
  components() |>
  gg_season(season_year)


augment(fit) |>
  select(-.model) |>
  model(STL(.fitted)) |>
  components() |>
  gg_season(season_year)


fit |>
  forecast(h = "5 years") |>
  autoplot(us_gasoline, level = 80)
fit |>
  forecast(h = "5 years") |>
  mutate(hilo(Barrels, 80))

gg_tsresiduals(fit, lag = 150)
