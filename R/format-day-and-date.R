format_day_and_date <- function(day, date) {
  glue::glue(
    '<div class="day-and-date">',
    '<div class="day-of-week">{day}</div>',
    '<div>{date}</div>',
    '</div>'
  )
}
