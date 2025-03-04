#2. Download data and create tsibble
library(fpp3)

aus_accommodation <- read.csv(
  "https://workshop.nectric.com.au/user2024/data/aus_accommodation.csv"
) |>
  mutate(Date = as.Date(Date)) |>
  as_tsibble(
    index = Date,
    key = State
  )
aus_accommodation

#3. Adjust frequency
aus_accommodation <- read.csv(
  "https://workshop.nectric.com.au/user2024/data/aus_accommodation.csv"
) |>
  mutate(Quarter = yearquarter(as.Date(Date))) |>
  as_tsibble(
    index = Quarter,
    key = State
  )
aus_accommodation

#4a Total quarterly visitors to Victoria
tourism |>
  filter(State == "Victoria") |>
  summarise(Trips = sum(Trips))

#5. Find combination of Region and Purpose with maximum number of overnight trips on average
tourism |>
  as_tibble() |>
  group_by(Region, Purpose) |>
  summarise(Trips = mean(Trips), .groups = "drop") |>
  filter(Trips == max(Trips))

#6. Create new tsibble with total number of trips by state
tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips))
