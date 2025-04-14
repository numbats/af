library(fpp3)

vic_pigs <- aus_livestock |>
  filter(
    Animal == "Pigs",
    State == "Victoria"
  )

vic_pigs |>
  autoplot(Count)

vic_pigs |>
  model(STL(Count)) |>
  components() |>
  autoplot()

fit <- vic_pigs |>
  model(
    ETS(Count ~ error("A") + trend("N") + season("N")),
    NAIVE(Count),
    MEAN(Count)
  )
fit |>
  forecast() |>
  autoplot(
    filter(vic_pigs, Month >= yearmonth("2016 Jan")),
    level = 50, alpha = 0.3
  )

augment(fit)
tidy(fit)


alpha = 0.2
x <- 0:99
weights <- alpha * (1 - alpha) ^ x
plot(x,weights, type = "l")










PBS |>
  summarise(Cost = sum(Cost)) |>
  autoplot()

PBS |>
  summarise(Cost = sum(Cost)) |>
  model(ETS(Cost))

global_economy |>
  filter(Country == "China") |>
  autoplot(GDP)

global_economy |>
  filter(Country == "China") |>
  model(ETS(GDP)) |>
  components() |>
  autoplot()
