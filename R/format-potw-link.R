format_potw_link <- function(id, resource) {
  id <- stringr::str_remove(id, "potw-0{0,1}")

  glue::glue(
    '<div class="hidden-placeholder">Placeholder</div>',
    '<div><a href="{resource}">',
    '{fontawesome::fa("calendar-week")} Problems of the week&nbsp;{id}',
    '</a></div>'
  )
}
