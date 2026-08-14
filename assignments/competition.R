library(tidyverse)

# Load some functions
source(here::here("assignments/competition_functions.R"))

# Read responses
competition <- readRDS(here::here("assignments/competition_responses.rds"))

# Actual values
q1 <- q2 <- q3 <- q4 <- q5 <- NULL
q1 <- 82247.29 / 1e6 # https://portwatch.imf.org/pages/cb5856222a5b4105adc6ee7e880a1730
#q2 <- #https://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;opposition=2;opposition=25;orderby=start;team=2;team=25;template=results;type=team;view=match
#q3 <- #https://www.bom.gov.au/climate/dwo/IDCJDW3049.latest.shtml
#q4 <- #https://finance.yahoo.com/quote/GOOG/history/
#q5 <- #https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release

# Create leaderboard
leaders <- tibble(
  Name = competition[["Name"]],
) |>
  bind_cols(
    score(
      1,
      q1,
      competition[["Q1F"]],
      competition[["Q1L"]],
      competition[["Q1U"]]
    ),
    score(
      2,
      q2,
      competition[["Q2F"]],
      competition[["Q2L"]],
      competition[["Q2U"]]
    ),
    score(
      3,
      q3,
      competition[["Q3F"]],
      competition[["Q3L"]],
      competition[["Q3U"]]
    ),
    score(
      4,
      q4,
      competition[["Q4F"]],
      competition[["Q4L"]],
      competition[["Q4U"]]
    ),
    score(
      5,
      q5,
      competition[["Q5F"]],
      competition[["Q5L"]],
      competition[["Q5U"]]
    )
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
competition |>
  plotcompetition(
    F = Q1F,
    L = Q1L,
    U = Q1U,
    Actual = q1,
    xlab = "millions metric tons"
  ) +
  ggtitle("Trade volume Str Hormuz 7 Aug 2026")
dev.off()

savepng(here::here("assignments/Q2"), height = 80, width = 15)
competition |>
  plotcompetition(
    F = Q2F,
    L = Q2L,
    U = Q2U,
    Actual = q2,
    xlab = "Run difference (Australia - Bangladesh)"
  ) +
  ggtitle("Second cricket test Aus v Bang 2026")
dev.off()

savepng(here::here("assignments/Q3"), height = 80, width = 15)
competition |>
  plotcompetition(
    F = Q3F,
    L = Q3L,
    U = Q3U,
    Actual = q3,
    xlab = "Temperature (°C)"
  ) +
  ggtitle("Max temp Melb airport 13 Sep 2026")
dev.off()

savepng(here::here("assignments/Q4"), height = 80, width = 15)
competition |>
  plotcompetition(F = Q4F, L = Q4L, U = Q4U, Actual = q4, xlab = "US dollars") +
  ggtitle("Google stock price 5 Oct 2026")
dev.off()

savepng(here::here("assignments/Q5"), height = 80, width = 15)
competition |>
  plotcompetition(F = Q5F, L = Q5L, U = Q5U, Actual = q5, xlab = "Millions") +
  ggtitle("Seas adj total employment Sep 2026") +
  coord_cartesian(xlim = c(12, 18))
dev.off()
