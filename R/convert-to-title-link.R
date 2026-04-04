source(here::here("R", "convert-to-title.R"))

convert_to_title_link <- function(path) {
  purrr::map_chr(path, \(path) {
    if (is.na(path)) {
      return(NA)
    }

    title <- convert_to_title(path)

    if (is.na(title)) {
      return(NA)
    }

    paste0("[", title, "](", path, ")")
  })
}
