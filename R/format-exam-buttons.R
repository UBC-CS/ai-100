source(here::here("R", "format-id.R"))

format_exam_buttons <- function(id, resource) {
  id <- format_id(id)
  book <- glue::glue("Book {id} on PrairieTest")
  practice <- glue::glue("Practice for {id} on PrairieLearn")

  glue::glue(
    '<a class="exam-button" href="https://us.prairietest.com" ',
    'title="{practice}" aria-label="{book}">Book</a> ',
    '<a class="exam-button" href="{resource}" ',
    'title="{practice}" aria-label="{practice}">Practice</a>'
  )
}
