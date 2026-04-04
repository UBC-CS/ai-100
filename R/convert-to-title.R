convert_to_title <- function(path) {
  purrr::map_chr(path, \(path) {
    if (is.na(path)) {
      return(NA)
    }
    file_yaml <- path |>
      rmarkdown::yaml_front_matter()

    title <- file_yaml |>
      purrr::pluck("title")

    subtitle <- file_yaml |>
      purrr::pluck("subtitle")

    title <- ifelse(
      !is.null(title),
      stringr::str_remove(title, "\\{\\{<.*>\\}\\} "),
      NA
    )

    subtitle <- ifelse(
      !is.null(subtitle),
      stringr::str_remove(subtitle, ",.*"),
      NA
    )

    if (all(is.na(c(subtitle, title)))) {
      return(NA)
    }

    if (is.na(subtitle)) {
      return(title)
    }

    subtitle <- glue::glue('<span class="unit-title">{subtitle}</span>')

    if (is.na(title)) {
      return(subtitle)
    }

    paste0(subtitle, "\n", title)
  })
}
