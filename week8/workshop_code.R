library(fpp3)

# AUD exchange rates

aud_xr <- readRDS(url("https://bit.ly/aud_xr"))

aud_xr |>
  distinct(Currency, Description)

aud_xr |>
  filter(Currency == ???) |>
  autoplot(Rate)

aud_xr |>
  filter(Currency == ???) |>
  autoplot(difference(Rate))

aud_xr |>
  features(Rate, unitroot_ndiffs)


# Tasmanian accommodation takings

tas_takings <- aus_accommodation |>
  filter(State == "Tasmania") |>
  select(Takings)

tas_takings |>
  autoplot(
    box_cox(Takings, lambda = ???)
  )

  tas_takings |>
  autoplot(
    box_cox(Takings, lambda = ???) |>
    difference(lag = ???)
  )


tas_takings |>
  autoplot(
    box_cox(Takings, lambda = ???) |>
    difference(lag = ???) |>
    difference(lag = ???)
  )


tas_takings |>
  ACF(
    box_cox(Takings, lambda = ???) |>
    difference(lag = ???) |>
    difference(lag = ???)
  ) |>
  autoplot()

tas_takings |>
  features(
    box_cox(Takings, lambda = ???),
    unitroot_nsdiffs
  )


tas_takings |>
  features(
    box_cox(Takings, lambda = ???) |> difference(lag = ???),
    unitroot_nsdiffs
  )
