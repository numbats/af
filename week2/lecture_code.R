library(fpp3)

aus_production |>
  filter(year(Quarter) >= 1980) |>
  autoplot(Electricity) +
  labs(
    y = "GWh",
    title = "Australian electricity production"
  )

aus_production |>
  autoplot(Tobacco) +
  labs(
    title = "Australian tobacco production",
    y = "million units"
  )

aus_production |>
  autoplot(Bricks) +
  labs(
    title = "Australian clay brick production",
    y = "million units"
  )

us_employment |>
  filter(Title == "Retail Trade", year(Month) >= 1980) |>
  autoplot(Employed / 1e3) +
  labs(
    title = "Retail employment, USA",
    y = "Million people"
  )

gafa_stock |>
  filter(Symbol == "AMZN", year(Date) >= 2018) |>
  autoplot(Close) +
  labs(
    title = "Amazon closing stock price",
    y = "$US"
  )

gafa_stock |>
  filter(Symbol == "AMZN", year(Date) >= 2018) |>
  ggplot(aes(x = Date, y = Close)) +
  geom_line() +
  labs(
    title = "Amazon closing stock price",
    y = "$US"
  )


## Snowy mountains tourism -------------------------------------------------------------------------

snowy <- tourism |>
  filter(Region == "Snowy Mountains") |>
  summarise(Trips = sum(Trips))
snowy |> autoplot(Trips)

snowy |> gg_season(Trips, labels="both")
snowy |> gg_subseries(Trips)

snowy |> gg_lag(Trips)
snowy |> gg_lag(Trips, geom = "point", lags = 1:16)
snowy |> ACF(Trips, lag_max = 16)
snowy |>
  ACF(Trips, lag_max = 20) |>
  autoplot()
snowy |>
  ACF(Trips) |>
  autoplot()


## RETAIL TRADE ------------------------------------------------------------------

retail <- us_employment |>
  filter(Title == "Retail Trade", year(Month) >= 1980)
retail |> autoplot(Employed)

retail |>
  ACF(Employed, lag_max = 48) |>
  autoplot()

# Pelts

pelt |>
  autoplot(Lynx) +
  labs(
    title = "Annual Canadian Lynx Trappings",
    y = "Number trapped"
  )

pelt |>
  ACF(Lynx, lag_max = 25) |>
  autoplot()


## WHITE NOISE --------------------------------------------------------------------

set.seed(30)
wn <- tsibble(t = seq(50), y = rnorm(50), index = t)
wn |> autoplot(y)

wn |> ACF(y, lag_max = 10)

wn |> ACF(y) |> autoplot()

## PIGS ---------------------------------------------------------------------------

pigs <- aus_livestock |>
  filter(
    State == "Victoria", Animal == "Pigs",
    year(Month) >= 2014
  )
pigs |> autoplot(Count / 1e3) +
  labs(
    y = "Thousands",
    title = "Number of pigs slaughtered in Victoria"
  )
pigs |>
  ACF(Count, lag_max = 36) |>
  autoplot()


## Google 2015 -------------------------------------------------------------------

google_2015 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2015) |>
  select(Date, Close)

google_2015 |> autoplot(Close)

google_2015 |>
  ACF(Close, lag_max = 100) |>
  autoplot()

google_2015 |>
  mutate(diff = difference(Close)) |>
  autoplot(diff)

google_2015 |>
  mutate(diff = difference(Close)) |>
  ACF(diff, lag_max = 100) |>
  autoplot()


library(fpp3)

# NSW criminal offences

nsw_offences

nsw_offences |>
  count(Type)

nsw_offences |>
  filter(Type == "Homicide")

nsw_offences |>
  select(-Count)

nsw_offences |>
  summarise(total = sum(Count))

nsw_offences |>
  as_tibble() |>
  group_by(Type) |>
  summarise(Total = sum(Count)) |>
  arrange(desc(Total))

nsw_offences |>
  as_tibble() |>
  group_by(Month) |>
  reframe(
    Type = Type,
    Count = Count,
    PC = Count / sum(Count) * 100
  ) |>
  ungroup()


nsw_offences |>
  filter(Type == "Theft") |>
  autoplot(Count)

autoplot(nsw_offences, Count)

nsw_offences |>
  autoplot(Count) +
  theme(legend.position = "bottom")

nsw_offences |>
  gg_season(Count)

nsw_offences |>
  filter(Type == "Assault") |>
  gg_season(Count)

nsw_offences |>
  filter(Type == "Assault") |>
  gg_subseries(Count)

nsw_offences |>
  filter(Type == "Homicide") |>
  gg_season(Count)

nsw_offences |>
  filter(Type == "Theft") |>
  gg_subseries(Count)

nsw_offences |>
  filter(Type == "Malicious damage to property") |>
  autoplot()

nsw_offences |>
  filter(Type == "Transport regulatory offences") |>
  autoplot()


# Australian fertility

aus_fertility

aus_fertility |>
  filter(Region == "Australia") |>
  as_tibble() |>
  group_by(Year) |>
  summarise(Rate = sum(Rate)) |>
  ungroup() |>
  ggplot(aes(x = Year, y = Rate)) +
  geom_line()

#At what age do people have the most babies, and has that changed over time?

aus_fertility |>
  filter(Region == "Australia") |>
  mutate(Age = readr::parse_number(Age)) |>
  as_tibble() |>
  group_by(Age) |>
  summarise(Rate = sum(Rate)) |>
  filter(Rate == max(Rate))

aus_fertility |>
  filter(Region == "Australia") |>
  mutate(Age = readr::parse_number(Age)) |>
  summarise(most_babies = Age[which.max(Rate)]) |>
  autoplot(most_babies)


