######## Course info ########
library(tidyverse)

# Start of semester
start_semester <- "2025-03-03"

# Week of mid-semester break
mid_semester_break <- "2025-04-21"

# Schedule
schedule <- tibble(
  Week = seq(12),
  Topic = c(
    "Introduction to forecasting and R",
    "Time series graphics",
    "Time series decomposition",
    "Simple forecasting methods",
    "Accuracy evaluation",
    "Exponential smoothing",
    "Exponential smoothing",
    "ARIMA models",
    "ARIMA models",
    "ARIMA models",
    "Multiple regression and forecasting",
    "Dynamic regression"
  ),
  Chapter = c(
    "1. Getting started",
    "2. Time series graphics",
    "3. Time series decomposition",
    "5. The forecaster's toolbox",
    "5. The forecaster's toolbox",
    "8. Exponential smoothing",
    "8. Exponential smoothing",
    "9. ARIMA models",
    "9. ARIMA models",
    "9. ARIMA models",
    "7. Time series regression models",
    "10. Dynamic regression models"
  ),
  Chapter_URL = c(
    "https://OTexts.com/fpp3/intro.html",
    "https://OTexts.com/fpp3/graphics.html",
    "https://OTexts.com/fpp3/decomposition.html",
    "https://OTexts.com/fpp3/toolbox.html",
    "https://OTexts.com/fpp3/toolbox.html",
    "https://OTexts.com/fpp3/expsmooth.html",
    "https://OTexts.com/fpp3/expsmooth.html",
    "https://OTexts.com/fpp3/arima.html",
    "https://OTexts.com/fpp3/arima.html",
    "https://OTexts.com/fpp3/arima.html",
    "https://OTexts.com/fpp3/regression.html",
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
    # Week =
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
  7 * floor(as.numeric(x - 1 + 4) / 7) + as.Date(1 - 4, origin = "1970-01-01")
}

assignments <- read_csv(here::here("assignments.csv")) |>
  mutate(
    Date = lastmon(Due),
    Moodle = paste0(
      "https://learning.monash.edu/mod/",
      c("quiz", rep("assign", 4)), "/view.php?id=", Moodle
    ),
    File = paste0("assignments/", File)
  )

quizzes <- read_csv(here::here("quizzes.csv")) |>
  mutate(
    Date = lastmon(QDue),
    QMoodle = paste0(
      "https://learning.monash.edu/mod/quiz/view.php?id=",
      QMoodle
    )
  )

schedule <- schedule |>
  left_join(assignments, by = "Date") |>
  left_join(quizzes, by = "Date")

show_assignments <- function(week) {
  today <- Sys.Date()
  monday <- schedule |>
    filter(Week == week) |>
    pull(Date) |>
    as.Date()
  # Show assignments and quizzes up to 2 weeks ahead
  # Show all assignments when in last 3 weeks of semester
  if (today > as.Date("2025-05-04") | (monday - today) <= 7 * 2 | week < 3) {
    ass <- schedule |>
      filter(
        Week >= week,
        Week < week + 3,
        !is.na(Assignment),
      ) |>
      select(Assignment:File)
    if (NROW(ass) > 0) {
      cat("\n\n## Assignments\n\n")
      for (i in seq(NROW(ass))) {
        cat("* [", ass$Assignment[i], "](../", ass$File[i], ") is due on ",
          format(ass$Due[i], "%A %d %B.\n"),
          sep = ""
        )
      }
    }
    show_quiz(week)
  }
}

show_quiz <- function(week) {
  ass <- schedule |>
    filter(Week == week, !is.na(Quiz)) |>
    select(Quiz:QMoodle)
  if (NROW(ass) > 0) {
    cat("\n\n## Weekly quiz\n\n")
    for (i in seq(NROW(ass))) {
      cat("* [", ass$Quiz[i], " quiz](", ass$QMoodle[i], ") is due on ",
        format(ass$QDue[i], "%A %d %B.\n"),
        sep = ""
      )
    }
  }
}

show_slides <- function(week) {
  today <- Sys.Date()
  monday <- monday <- schedule |>
    filter(Week == week) |>
    pull(Date) |>
    as.Date()
  # Show slides one week ahead
  if ((monday - today) <= 7 | week <= 1) {
    file <- paste0("https://af.numbat.space/week", week, "/slides.pdf")
    embed <- paste0(
      "<iframe src='https://docs.google.com/gview?url=",
      file,
      "&embedded=true' width='100%' height=465></iframe>"
    )
    button <- paste0("<a href=", file, " class='badge badge-small badge-red'>Download pdf</a>")
    cat(paste0("## Slides for Monday lecture\n\n", embed, "\n", button))
  }
}


show_activity <- function(week, title = TRUE, show_solutions = TRUE) {
  today <- Sys.Date()
  monday <- monday <- schedule |>
    filter(Week == week) |>
    pull(Date) |>
    as.Date()
  # Show slides one week ahead
  if ((monday - today) <= 7 | week <= 1) {
    file <- here::here(paste0("week", week, "/activities.qmd"))
    if (fs::file_exists(file)) {
      cat("\n\n## [Activities for Tuesday workshop](activities.qmd)\n\n")
    }
  }
}

submit <- function(schedule, assignment) {
  ass <- schedule |>
    filter(Assignment == assignment)
  due <- format(ass$Due, "%e %B %Y") |> stringr::str_trim()
  url <- ass$Moodle
  button <- paste0(
    "<br><br><hr><b>Due: ", due, "</b><br>",
    "<a href=", url, " class = 'badge badge-large badge-blue'>",
    "<font size='+2'>&nbsp;&nbsp;<b>Submit</b>&nbsp;&nbsp;</font><br></a>"
  )
  cat(button)
}
