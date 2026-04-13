source(here::here("R", "format-id.R"))
source(here::here("R", "get-id-title.R"))

format_resource_as_label <- function(show_week, id, type, resource) {
  if (is.na(type)) {
    return(NA)
  }

  lookup <- readr::read_csv(here::here("data", "lookup.csv"), col_types = "cc")

  types_as_icons <- lookup |>
    dplyr::filter(!is.na(font_awesome_icon)) |>
    dplyr::pull(type)

  if (stringr::str_detect(id, "exam")) {
    return(format_id(id))
  }

  if (type %in% types_as_icons) {
    return(format_resource_as_icon(show_week, id, type, resource))
  }

  if (stringr::str_detect(id, "part")) {
    return(get_id_title(resource, as_link = TRUE))
  }

  glue::glue('<a href="{resource}">{format_id(id)}</a>')
}

format_resource_as_icon <- function(show_week, id, type, resource) {
  id <- format_id(id)
  type <- type |> as.character()
  lookup <- readr::read_csv(
    here::here("data", "lookup.csv"),
    col_types = "cc"
  ) |>
    dplyr::filter(!is.na(font_awesome_icon))

  title <- stringr::str_c(
    id,
    type |> dplyr::recode_values(from = lookup$type, to = lookup$label),
    sep = " "
  )

  icon <- type |>
    dplyr::recode_values(
      from = lookup$type,
      to = lookup$font_awesome_icon
    ) |>
    fontawesome::fa()

  if (!show_week) {
    return(
      glue::glue('<span class="inactive-link">{icon}</span>')
    )
  }

  glue::glue(
    '<a href="{resource}" title="{title}" aria-label="{title}">{icon}</a>'
  )
}
