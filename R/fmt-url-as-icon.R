fmt_url_as_icon <- function(data) {
  lookup <- readr::read_csv(
    here::here("data", "lookup.csv"),
    col_types = "cc"
  ) |>
    dplyr::filter(!is.na(font_awesome_icon)) |>
    dplyr::mutate(type = stringr::str_to_snake(type))

  data |>
    purrr::reduce2(
      lookup$type,
      lookup$font_awesome_icon,
      \(data, type, font_awesome_icon) {
        data |>
          gt::sub_values(
            columns = tidyselect::ends_with(type),
            rows = !show_week,
            pattern = ".*",
            replacement = fontawesome::fa(
              font_awesome_icon,
              fill_opacity = 0.1
            ),
            escape = FALSE
          ) |>
          gt::fmt_url(
            columns = tidyselect::ends_with(type),
            rows = show_week,
            label = fontawesome::fa(font_awesome_icon)
          )
      },
      .init = _
    )
}
