library(fpp3)

xr_meta <- readr::read_csv(here::here("week8/xr_meta.csv"))
aud_xr <- readr::read_csv(here::here("week8/aud_xr.csv")) |>
  pivot_longer(-Date, names_to = "Currency", values_to = "Rate") |>
  filter(!Currency %in% c("SDR", "Index")) |>
  mutate(Date = dmy(Date)) |>
  left_join(xr_meta, by = "Currency") |>
  arrange(Currency, Date) |>
  group_by(Currency) |>
  mutate(Days = row_number()) |>
  ungroup() |>
  as_tsibble(index = Days, key = Currency) |>
  filter(!is.na(Rate))

saveRDS(aud_xr, here::here("week8/aud_xr.rds"))
