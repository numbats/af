######## Course info ########
library(tidyverse)

# Start of semester
start_semester <- "2024-02-26"

# Week of mid-semester break
mid_semester_break <- "2024-04-01"

# Schedule
schedule <- tibble(
  Week = seq(12),
  Topic = c(
    "Introduction to forecasting and R",
    "Time series graphics",
    "Time series decomposition",
    "The forecaster's toolbox",
    "Exponential smoothing",
    "Exponential smoothing",
    "ARIMA models",
    "ARIMA models",
    "ARIMA models",
    "Multiple regression and forecasting",
    "Dynamic regression",
    "Dynamic regression"
  ),
  Chapter = c(
    "1. Getting started",
    "2. Time series graphics",
    "3. Time series decomposition",
    "5. The forecaster's toolbox",
    "8. Exponential smoothing",
    "8. Exponential smoothing",
    "9. ARIMA models",
    "9. ARIMA models",
    "9. ARIMA models",
    "7. Time series regression models",
    "10. Dynamic regression models",
    "10. Dynamic regression models"
  ),
  Chapter_URL = c(
    "https://OTexts.com/fpp3/intro.html",
    "https://OTexts.com/fpp3/graphics.html",
    "https://OTexts.com/fpp3/decomposition.html",
    "https://OTexts.com/fpp3/toolbox.html",
    "https://OTexts.com/fpp3/expsmooth.html",
    "https://OTexts.com/fpp3/expsmooth.html",
    "https://OTexts.com/fpp3/arima.html",
    "https://OTexts.com/fpp3/arima.html",
    "https://OTexts.com/fpp3/arima.html",
    "https://OTexts.com/fpp3/regression.html",
    "https://OTexts.com/fpp3/dynamic.html",
    "https://OTexts.com/fpp3/dynamic.html"
  )
)

# Add mid-semester break
calendar <- tibble(
    Date = seq(as.Date(start_semester), by = "1 week", length.out = 13)
  ) |>
  mutate(
    Week = row_number(),
    Week = if_else(Date < mid_semester_break, Week, Week - 1),
    #Week =
  )

# Add calendar to schedule
schedule <- schedule |>
  left_join(calendar, by = "Week") |>
  mutate(
    Week = if_else(Date == mid_semester_break, NA, Week),
    Topic = if_else(Date == mid_semester_break, "Mid-semester break", Topic),
    Chapter = if_else(Date == mid_semester_break, NA, Chapter),
    Chapter_URL = if_else(Date == mid_semester_break, NA, Chapter_URL)
  ) |>
  select(Week, Date, everything())

# Add assignment details
lastmon <- function(x) {
  7 * floor(as.numeric(x-1+4)/7) + as.Date(1-4, origin="1970-01-01")
}

assignments <- read_csv(here::here("assignments.csv")) |>
  mutate(
    Date = lastmon(Due),
    Moodle = paste0("https://learning.monash.edu/mod/assign/view.php?id=", Moodle),
    File = paste0("assignments/", File)
  )

schedule <- schedule |>
  left_join(assignments, by = "Date")

show_assignments <- function() {
  ass <- schedule |>
    filter(
      Week >= week,
      Week < week + 3,
      !is.na(Assignment),
    ) |>
    select(Assignment: Moodle)
  if(NROW(ass) > 0) {
    cat("\n\n## Assignments\n\n")
    for(i in seq(NROW(ass))) {
      cat("* [", ass$Assignment[i], "](", ass$File[i], ") is due on ",
          format(ass$Due[i], "%A %d %B.\n"), sep="")
    }
  }
}

show_slides <- function(week) {
  file <- paste0(
      "https://raw.githubusercontent.com/numbats/af/main/slides/week",
      week,
      ".pdf"
  )
  embed <- paste0(
      "<iframe src='https://docs.google.com/gview?url=",
      file,
      "&embedded=true' width='100%' height=465></iframe>"
    )
  button <- paste0("<a href=", file, " class='badge badge-small badge-red'>Download pdf</a>")
  cat(paste0("## Slides for seminar\n\n", embed,"\n", button))
}
