# Australian inbound arrivals

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
  group_by(Month) 


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
  gg_season(Count)

nsw_offences |>
  filter(Type == "Malicious damage to property") |>
  autoplot()

nsw_offences |>
  filter(Type == "Transport regulatory offences") |>
  autoplot()

# Which series is the spike in the latter part of the data?


# Australian fertility

aus_fertility

At what age do people have the most babies, and has that changed over time?

## Cyclic data

sunspot.year
pelt

## Weekly data

aus_mortality


## Daily data: Melbourne walkers

melb_walkers 
melb_walkers |> autoplot()
melb_walkers |> gg_season(Count)
melb_walkers |> gg_season(Count, period = "week")


## Daily data: OTexts page views

otexts_views 
otexts_views |> autoplot()
otexts_views |> gg_season(Pageviews)
otexts_views |> gg_season(Pageviews, period = "week")


# Sub-daily data

vic_elec


## Scatterplots

aus_production

aus_production |> autoplot(Cement)

aus_production |>
  pivot_longer(-Quarter, values_to = "value", names_to = "Commodity") |>
  autoplot(value)

aus_production |>
  pivot_longer(-Quarter, values_to = "value", names_to = "Commodity") |>
  autoplot(value) +
  facet_grid(Commodity ~ ., scales = "free_y")
  
aus_production |>
  as_tibble() |>
  select(-Quarter) |>
  GGally::ggpairs()

## Insurance with lagged advertising

ins <- insurance |>
  mutate(
    lagAds = lag(TVadverts), 
    lagAds2 = lag(TVadverts, 2)
  )

ins |> 
  as_tibble() |>
  select(TVadverts, lagAds, lagAds2, Quotes) |>
  GGally::ggpairs()


## Daily data and scatterplots

vic_elec_day_type <- vic_elec |> 
   filter(year(Time) == 2014) |> 
   mutate(Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"))
vic_elec_day_type

vic_elec_day_type |> 
   select(Temperature, Demand) |> 
   pivot_longer(-Time) |> 
   ggplot(aes(Time, value, colour = name)) +
   geom_line() +
   facet_grid(name ~ ., scales = "free_y") +
   guides(colour = "none") +
   labs(
    y = "Degrees Celsius                   GW         "
  )

vic_elec_day_type |> 
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() + 
   labs(x = "Temperature (degrees Celsius)", y = "Electricity demand (GW)")

vic_elec_day_type |> 
  ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
  geom_point() + 
   labs(x = "Temperature (degrees Celsius)", y = "Electricity demand (GW)")


## White noise 
wn <- tsibble(t = 1:100, y = rnorm(100), index = t)
wn |> autoplot(y)
wn |> ACF(y) |> autoplot()

## Random walks
rw <- tsibble(t = 1:100, y = cumsum(rnorm(100)), index = t)
rw |> autoplot(y)
rw |> ACF(y) |> autoplot()


## Lag plots

aus_migration

aus_migration |>
  filter(State == "VIC") |>
  autoplot(Births)

aus_migration |>
  filter(State == "VIC") |>
  ACF() |>
  autoplot()


aus_births

aus_births |>
  filter(State == "VIC") |>
  autoplot(Births)

aus_births |>
  filter(State == "VIC") |>
  ACF() |>
  autoplot()

