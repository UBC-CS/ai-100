source(here::here("R", "format-id.R"))
source(here::here("R", "get-id-from-resource.R"))
get_id_title <- function(resource, as_link = FALSE) {
  if (is.na(resource) | !fs::file_exists(resource)) {
    return(NA)
  }

  file_yaml <- rmarkdown::yaml_front_matter(resource)

  id <- file_yaml |>
    purrr::pluck("subtitle") %||%
    (get_id_from_resource(resource) |>
      format_id()) |>
    stringr::str_remove(",.*")

  title <- file_yaml |>
    purrr::pluck("title") %||%
    "Missing title" |>
    # Remove Quarto variables
    stringr::str_remove("\\{\\{<.*>\\}\\} ")

  if (as_link) {
    title <- glue::glue('<a href="{resource}">{title}</a>')
  }

  glue::glue(
    '<div class="unit-subtitle">{id}</div>',
    '<div>{title}</div>'
  )
}
