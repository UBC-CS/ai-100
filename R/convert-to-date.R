convert_to_date <- function(monday_of_first_term_week, week, slot) {
  purrr::map2_vec(
    week,
    slot,
    \(week, slot) {
      slot <- dplyr::replace_when(
        slot,
        slot %in% c("Week", "First", "Second", "Third") ~ "Mon"
      )

      (lubridate::ymd(monday_of_first_term_week) +
        lubridate::weeks(week - 1)) |>
        lubridate::ceiling_date(
          unit = "week",
          week_start = slot,
          change_on_boundary = FALSE
        )
    }
  )
}
