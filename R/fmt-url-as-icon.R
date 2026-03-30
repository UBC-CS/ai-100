fmt_url_as_icon <- function(data) {
  lookup <- readr::read_csv(here::here("data", "lookup.csv"), col_types = "cc")

  data |>
    purrr::reduce2(
      lookup$from,
      lookup$font_awesome_icon,
      \(data, from, font_awesome_icon) {
        data |>
          gt::fmt_url(
            columns = dplyr::ends_with(from),
            label = fontawesome::fa(font_awesome_icon)
          ) |>
          gt::sub_missing(
            columns = dplyr::ends_with(from),
            missing_text = fontawesome::fa(
              font_awesome_icon,
              fill_opacity = 0.1
            )
          )
      },
      .init = _
    )
}
