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

# Additive seasonality
sim |>
  autoplot(lt + st)

# Multiplicative seasonality
sim |>
  autoplot(2*lt + lt * st)

# Additive trend
sim |>
  autoplot(
    (seq_along(t) * bt)
  )

# Multiplicative trend
sim |>
  autoplot(
    lt * (seq_along(t) * bt)
  )

# Additive errors
sim |>
  autoplot(
    (seq_along(t) * bt) + et
    # trendcycle + N(0, 1) = N(trendcycle, 1)
  )

# Multiplicative errors
sim |>
  autoplot(
    2*lt + lt * et
    # lt * N(0, 1) = N(0, lt^2)
  )


# lt * bt * st * et
aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  autoplot(Turnover)

# log(lt * bt * st * et) = log(lt) + log(bt) + log(st) + log(et)
aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  autoplot(log(Turnover))


# exp(log(Turnover))

exp(-24)


log(y + 1)

log(1)

aus_production |>
  autoplot(Gas)

aus_production |>
  autoplot(box_cox(Gas, 0.11))

aus_production |>
  features(Gas, guerrero)

aus_production |>
  autoplot(log(Gas))

# 1. does it need a transform?
aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  autoplot(Turnover)


#2. find a simple math transform
aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  autoplot(log(Turnover))
# too strong

#3. weaker transformation
aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  autoplot(sqrt(Turnover))
# too weak

#4. just right?
aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  autoplot(box_cox(Turnover, 0.2))

aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  features(Turnover, guerrero)

# bad, need transform
aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  model(STL(Turnover)) |>
  components() |>
  autoplot()

# bad, need transform
aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  model(STL(box_cox(Turnover, 0.2) ~ trend(window = 33) +
              season(window = Inf))) |>
  components() |>
  autoplot()

canadian_gas |>
  model(STL(Volume ~ season(window = 7))) |>
  components() |>
  autoplot()
