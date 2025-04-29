library(fpp3)

# AUD exchange rates

aud_xr <- readRDS(url("https://bit.ly/aud_xr"))

aud_xr |>
  distinct(Currency, Description)

aud_xr |>
  filter(Currency == "IDR") |>
  gg_tsdisplay(Rate)
aud_xr |>
  filter(Currency == "IDR") |>
  mutate(diff = difference(Rate)) |>
  gg_tsdisplay(diff)

aud_xr |>
  features(Rate, unitroot_ndiffs)


# Tasmanian accommodation takings

tas_takings <- aus_accommodation |>
  filter(State == "Tasmania") |>
  select(Takings)

tas_takings |>
  autoplot(Takings)

tas_takings |>
  autoplot(
    box_cox(Takings, lambda = 0)
  )

tas_takings |>
  autoplot(
    box_cox(Takings, lambda = 0) |>
      difference(lag = 4)
  )

tas_takings |>
  autoplot(
    box_cox(Takings, lambda = 0) |>
      difference(lag = 4) |>
      difference(lag = 1)
  )

tas_takings |>
  ACF(
    box_cox(Takings, lambda = 0) |>
      difference(lag = 4) |>
      difference(lag = 1)
  ) |>
  autoplot()

tas_takings |>
  features(
    box_cox(Takings, lambda = 0),
    unitroot_nsdiffs
  )


tas_takings |>
  features(
    box_cox(Takings, lambda = 0) |> difference(lag = 4),
    unitroot_ndiffs
  )

aus_accommodation |>
  features(
    log(Takings),
    unitroot_nsdiffs
  )
aus_accommodation |>
  filter(State == "Australian Capital Territory") |>
  features(
    log(Takings),
    unitroot_ndiffs
  )
aus_accommodation |>
  filter(State != "Australian Capital Terrotory") |>
  features(
    log(Takings) |> difference(lag = 4),
    unitroot_ndiffs
  )
