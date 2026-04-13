source(here::here("R", "format-id.R"))

format_week_with_start_day <- function(date, id, resource) {
  id <- format_id(id)
  glue::glue(
    '<div class="week-with-start-date">',
    '<a href="{resource}">{id}</a>',
    '</div>',
    '<div class="start-date">starts {date}</div>'
  )
}
