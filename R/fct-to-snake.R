fct_to_snake <- function(factor) {
  levels <- factor |>
    levels() |>
    stringr::str_to_snake()
  factor |>
    as.character() |>
    stringr::str_to_snake() |>
    forcats::fct(levels = levels)
}
