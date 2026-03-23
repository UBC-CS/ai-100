convert_to_title_link <- function(path) {
  purrr::map_chr(path, \(path) {
    title <- path |>
      rmarkdown::yaml_front_matter() |>
      purrr::pluck("title")

    glue::glue("[{title}]({path})")
  })
}
