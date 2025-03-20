library(fpp3)

sim <- tibble(
  t = 1:144,
  lt = t/10,
  st = sin(t*2*pi/12),
  bt = 0.4,
  et = rnorm(length(t))
) |>
  as_tsibble(
    index = t
  )

sim |>
  autoplot(lt + st)

sim |>
  autoplot(2*lt + lt * st)

aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  autoplot(Turnover)
