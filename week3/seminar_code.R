library(fpp3)

# Australian monthly electricity production
elec1 <- as_tsibble(fma::elec) |>
  transmute(Month = index, GWh = value)

elec1 |>
  autoplot(box_cox(GWh, lambda = 0.3))

dcmp <- elec1 |>
  model(stl = STL(box_cox(GWh, lambda = 0.3)))

dcmp |>
  components() |>
  autoplot()

components(dcmp) |>
  gg_subseries(season_year)

elec1 |>
  autoplot(GWh, color = "gray") +
  autolayer(components(dcmp), inv_box_cox(trend, lambda = 0.3), color = "red")

elec1 |>
  autoplot(GWh, color = "gray") +
  autolayer(components(dcmp), inv_box_cox(season_adjust, lambda = 0.3), color = "red")

elec1 |>
  model(STL(log(GWh) ~ season(window = 3333) + trend(window = 7), robust = TRUE)) |>
  components() |>
  autoplot()


## Recent data


# Monthly electricity production
elec2 <- readr::read_csv(here::here("week3/MES_1123.csv"),
                         skip = 8) |>
  filter(Country == "Australia",
         Product == "Electricity",
         Balance == "Net Electricity Production"
  ) |>
  transmute(
    Month = yearmonth(as_date(paste("1",Time), format = "%d %B %Y")),
    GWh = Value
  ) |>
  as_tsibble(index = Month)

elec2 |>
  autoplot(GWh)

# Find last 14 years of data from previous data set
elec1 <- elec1 |> filter(year(Month) >= 1982)
elec1 |> gg_season(GWh)

elec2 |> gg_season(GWh)

elec1 |> gg_subseries(GWh)
elec2 |> gg_subseries(GWh)
