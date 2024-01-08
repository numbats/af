library(tidyverse)
library(readxl)
library(tsibble)

# xlsx file downloaded on 8 January.
# Need to download latest file on 6 Feb 2024 from
# https://www.abs.gov.au/statistics/industry/retail-and-wholesale-trade/retail-trade-australia/dec-2023/8501011.xlsx
download.file(
  url = "https://www.abs.gov.au/statistics/industry/retail-and-wholesale-trade/retail-trade-australia/oct-2023/8501011.xlsx",
  destfile = here::here("assignments/8501011.xlsx")
)

# Read data and meta data from xlsx file
series <- read_excel(here::here("assignments/8501011.xlsx"), sheet = 2, skip = 9) |>
  rename(Month = `Series ID`) |>
  gather(`Series ID`, Turnover, -Month) |>
  mutate(Month = yearmonth(Month))
dict <- read_excel(here::here("assignments/8501011.xlsx"), sheet = 1, skip = 9) |>
  filter(`Series Type` == "Original") |>
  separate(`Data Item Description`, c("Category", "State", "Industry"), sep = ";", extra = "drop") |>
  transmute(
    State = trimws(State),
    Industry = trimws(Industry),
    `Series ID`
  ) |>
  filter(
    Industry  != "Total (Industry)",
    State != "Total (State)"
  )

# Construct tibble
aus_retail <- left_join(dict, series, by = "Series ID") |>
  filter(!is.na(Turnover)) |>
  as_tibble()

# Make sure series go right to end
short_series <- aus_retail |>
  summarise(Month = max(Month), .by="Series ID") |>
  filter(Month < max(Month)) |>
  pull(`Series ID`)

aus_retail <- aus_retail |>
  filter(!(`Series ID` %in% short_series))

# Turn into tsibble
aus_retail <- aus_retail |>
  as_tsibble(
    index = Month,
    key = c(State, Industry)
  )

# Save data
saveRDS(aus_retail, here::here("assignments/retail.rds"))
