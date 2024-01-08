library(fpp3)

# CSV file downloaded from
# https://datacatalog.worldbank.org/search/dataset/0037712/World-Development-Indicators
# on 8 January 2024
tmp <- tempdir()
csv <- tempfile(fileext = ".csv")
download.file("https://databank.worldbank.org/data/download/WDI_CSV.zip", paste0(tmp,"/WDI_CSV.zip"))
unzip(paste0(tmp,"/WDI_CSV.zip"), exdir = tmp)
wdi <- readr::read_csv(paste0(tmp,"/WDIData.csv"))

# Read data, extract populations
pop <- wdi |>
  pivot_longer(`1960`:`2022`, names_to = "Year", values_to = "Value") |>
  filter(!is.na(Value)) |>
  select(-`...68`, -`Indicator Code`) |>
  pivot_wider(values_from = Value, names_from = `Indicator Name`) |>
  mutate(Year = as.numeric(Year)) |>
  transmute(
    Country = factor(`Country Name`),
    Code = factor(`Country Code`),
    Year = Year,
    Population = `Population, total`
  ) |>
  as_tsibble(key = Country, index = Year)

# Save data
pop |>
  saveRDS(here::here("assignments/pop.rds"))
