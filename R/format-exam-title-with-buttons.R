source(here::here("R", "format-id.R"))
source(here::here("R", "format-exam-buttons.R"))

format_exam_title_with_buttons <- function(id, resource) {
  buttons <- format_exam_buttons(id, resource)
  id <- format_id(id)

  glue::glue(
    '<div class="hidden-placeholder">Placeholder</div>',
    '<div class="exam-label">{id} {buttons}</div>'
  )
}
