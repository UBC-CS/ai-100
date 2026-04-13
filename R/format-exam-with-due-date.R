source(here::here("R", "format-id.R"))
source(here::here("R", "format-exam-buttons.R"))

format_exam_with_due_date <- function(
  day,
  date,
  show_week,
  show_exam,
  id,
  resource
) {
  add_buttons <- show_week | show_exam
  buttons <- format_exam_buttons(id, resource)

  id <- format_id(id)

  exam_with_due_date <- glue::glue(
    '<div class="exam-label">{id}</div>',
    '<div class="due-date">Due: {day} {date}</div>'
  )

  exam_with_due_date_and_buttons <- glue::glue(
    '{exam_with_due_date}',
    '<div class="exam-label">{buttons}</div>'
  )

  dplyr::if_else(
    add_buttons,
    exam_with_due_date_and_buttons,
    exam_with_due_date
  )
}
