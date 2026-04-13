fct_to_lower <- function(factor) {
  levels <- factor |>
    levels() |>
    stringr::str_to_lower()
  factor |>
    stringr::str_to_lower() |>
    forcats::fct(levels = levels)
}
