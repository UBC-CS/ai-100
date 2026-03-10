convert_to_date <- function(monday_of_first_term_week, week, day) {
  purrr::map2_vec(
    week,
    day,
    \(week, day) {
      (lubridate::ymd(monday_of_first_term_week) +
        lubridate::weeks(week - 1)) |>
        lubridate::ceiling_date(
          unit = "week",
          week_start = day,
          change_on_boundary = FALSE
        )
    }
  )
}
