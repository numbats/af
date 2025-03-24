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


