library(fpp3)

#1. Download data
file <- tempfile(fileext = ".xlsx")
download.file("https://robjhyndman.com/data/tourism.xlsx", file)
mytourism <- readxl::read_excel(file)

#2. Create a tsibble
mytourism <- mytourism |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(index = Quarter, key = c(Region, State, Purpose))

#3. Find combination of Region and Purpose with maximum number of overnight trips on average
my_tourism |>
  as_tibble() |>
  group_by(Region, Purpose) |>
  summarise(Trips = mean(Trips), .groups="drop") |>
  filter(Trips == max(Trips))

#4. Create new tsibble with total number of trips by state
state_tourism <- my_tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips))
