highlight_current_week <- function(current_week, element) {
  dplyr::if_else(
    current_week,
    glue::glue('<div class="current-week">{element}</div>', .na = NULL),
    element
  )
}
