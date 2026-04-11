get_id_from_resource <- function(resource) {
  resource |>
    fs::path_file() |>
    stringr::str_extract("^[^_]+")
}
