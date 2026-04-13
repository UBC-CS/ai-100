format_day_and_date_due <- function(day, date) {
  day <- glue::glue("Due: {day}")
  day_and_date <- format_day_and_date(day, date)

  glue::glue(
    '<div class="day-and-date-due">{day_and_date}</div>'
  )
}
