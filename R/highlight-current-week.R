highlight_current_week <- function(data) {
  current_week_highlight_style <- htmltools::css(
    background.color = "rgba(var(--bs-warning-rgb), 0.15)"
  )

  data <- data |>
    gt::tab_style(
      style = current_week_highlight_style,
      locations = gt::cells_body(
        rows = current_week
      )
    ) |>
    gt::tab_style(
      style = paste0(
        htmltools::css(vertical.align = "top"),
        current_week_highlight_style
      ),
      locations = gt::cells_body(
        columns = tidyselect::starts_with(c("day", "title")),
        rows = current_week
      )
    )
}
