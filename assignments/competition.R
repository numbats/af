library(tidyverse)

# Load some functions
source(here::here("assignments/competition_functions.R"))

# Read responses
competition <- readRDS(here::here("assignments/competition_responses.rds"))

# Actual values
q1 <- q2 <- q3 <- q4 <- q5 <- NULL
q1 <- 169.93 # https://finance.yahoo.com/quote/GOOG/history
#q2 <- 17.6 #http://www.bom.gov.au/climate/dwo/IDCJDW3049.latest.shtml
#q3 <- 0
#q4 <- 14.300000 #https://www.abs.gov.au/ausstats/abs@.nsf/mf/6202.0
#q5 <- 178.00 # https://finance.yahoo.com/quote/GOOG/history

# Create leaderboard
leaders <- tibble(
    Name = competition[["Name"]],
  ) |>
  bind_cols(
    score(1, q1, competition[["Q1F"]], competition[["Q1L"]], competition[["Q1U"]]),
    score(2, q2, competition[["Q2F"]], competition[["Q2L"]], competition[["Q2U"]]),
    score(3, q3, competition[["Q3F"]], competition[["Q3L"]], competition[["Q3U"]]),
    score(4, q4, competition[["Q4F"]], competition[["Q4L"]], competition[["Q4U"]]),
    score(5, q5, competition[["Q5F"]], competition[["Q5L"]], competition[["Q5U"]])
  ) |>
  rowwise() |>
  mutate(Score = sum(c_across(-Name))) |>
  select(Name, Score, everything()) |>
  arrange(Score, Name)

# Save leaderboard
saveRDS(leaders, here::here("assignments/competition_leaderboard.rds"))

# Plotting
ggplot2::theme_set(
  theme_get() + theme(text = element_text(family = 'Fira Sans'))
)

# Plot responses
savepng(here::here("assignments/Q1"), height = 80, width = 15)
competition |> plotcompetition(F = Q1F, L = Q1L, U = Q1U, Actual = q1, xlab = "US dollars") +
  ggtitle("Google stock price 24 March 2025")
dev.off()

savepng(here::here("assignments/Q2"), height = 80, width = 15)
competition |> plotcompetition(F = Q2F, L = Q2L, U = Q2U, Actual = q2, xlab = "degrees C") +
  ggtitle("Maximum temp at airport on 14 April 2025")
dev.off()

savepng(here::here("assignments/Q3"), height = 80, width = 15)
competition |> plotcompetition(
  F = Q3F, L = Q3L, U = Q3U, Actual = q3,
  xlab = "Point difference (Collingwood - Essendon)"
) +
  ggtitle("Difference in points Anzac Day match")
dev.off()

savepng(here::here("assignments/Q4"), height = 80, width = 15)
competition |> plotcompetition(F = Q4F, L = Q4L, U = Q4U, Actual = q4, xlab = "Millions") +
  ggtitle("Seasonally adjusted total employment in April 2025") 
dev.off()

savepng(here::here("assignments/Q5"), height = 80, width = 15)
competition |> plotcompetition(F = Q5F, L = Q5L, U = Q5U, Actual = q5, xlab = "US dollars") +
  ggtitle("Google stock price 26 May 2025")
dev.off()
