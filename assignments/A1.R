library(tidyverse)

# Load some functions
source(here::here("assignments/A1_functions.R"))

# Read responses
ass1 <- readRDS(here::here("assignments/A1_responses.rds"))

# Actual values
q1 <- q2 <- q3 <- q4 <- q5 <- NULL
q1 <- 149.68 # https://finance.yahoo.com/quote/GOOG/history
q2 <- 17.6 #http://www.bom.gov.au/climate/dwo/IDCJDW3049.latest.shtml
q3 <- 0
q4 <- 14.300000 #https://www.abs.gov.au/ausstats/abs@.nsf/mf/6202.0
#q5 <- ???? # https://finance.yahoo.com/quote/GOOG/history

# Create leaderboard
leaders <- tibble(
    Name = ass1[["Name"]],
  ) |>
  bind_cols(
    score(1, q1, ass1[["Q1F"]], ass1[["Q1L"]], ass1[["Q1U"]]),
    score(2, q2, ass1[["Q2F"]], ass1[["Q2L"]], ass1[["Q2U"]]),
    score(3, q3, ass1[["Q3F"]], ass1[["Q3L"]], ass1[["Q3U"]]),
    score(4, q4, ass1[["Q4F"]], ass1[["Q4L"]], ass1[["Q4U"]]),
    score(5, q5, ass1[["Q5F"]], ass1[["Q5L"]], ass1[["Q5U"]])
  ) |>
  rowwise() |>
  mutate(Score = sum(c_across(-Name))) |>
  select(Name, Score, everything()) |>
  arrange(Score, Name)

# Save leaderboard
saveRDS(leaders, here::here("assignments/A1_leaderboard.rds"))

# Plotting
ggplot2::theme_set(
  theme_get() + theme(text = element_text(family = 'Fira Sans'))
)

# Plot responses
savepng(here::here("assignments/Q1"), height = 80, width = 15)
ass1 |> plotass1(F = Q1F, L = Q1L, U = Q1U, Actual = q1, xlab = "US dollars") +
  ggtitle("Google stock price 20 March 2024")
dev.off()

savepng(here::here("assignments/Q2"), height = 80, width = 15)
ass1 |> plotass1(F = Q2F, L = Q2L, U = Q2U, Actual = q2, xlab = "degrees C") +
  ggtitle("Maximum temp at airport on 10 April 2024")
dev.off()

savepng(here::here("assignments/Q3"), height = 80, width = 15)
ass1 |> plotass1(
  F = Q3F, L = Q3L, U = Q3U, Actual = q3,
  xlab = "Point difference (Collingwood - Essendon)"
) +
  ggtitle("Difference in points Anzac Day match")
dev.off()

savepng(here::here("assignments/Q4"), height = 80, width = 15)
ass1 |> plotass1(F = Q4F, L = Q4L, U = Q4U, Actual = q4, xlab = "Millions") +
  ggtitle("Seasonally adjusted total employment in April 2024") +
  coord_cartesian(xlim=c(12, 15))
dev.off()

savepng(here::here("assignments/Q5"), height = 80, width = 15)
ass1 |> plotass1(F = Q5F, L = Q5L, U = Q5U, Actual = q5, xlab = "US dollars") +
  ggtitle("Google stock price 22 May 2024")
dev.off()
