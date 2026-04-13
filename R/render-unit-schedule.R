source(here::here("R", "fct-to-snake.R"))
source(here::here("R", "format-potw-link.R"))
source(here::here("R", "format-exam-title-with-buttons.R"))
source(here::here("R", "format-day-and-date.R"))
source(here::here("R", "format-day-and-date-due.R"))
source(here::here("R", "format-id.R"))
source(here::here("R", "format-resource-as-label.R"))
source(here::here("R", "get-id-title.R"))
source(here::here("R", "get-schedule.R"))
source(here::here("R", "highlight-current-week.R"))

render_unit_schedule <- function() {
  schedule <- get_schedule() |>
    dplyr::filter(!is.na(resource))

  row_groups <- schedule |>
    dplyr::filter(type == "summary") |>
    dplyr::mutate(
      title = purrr::pmap_chr(
        list(show_week, id, type, resource),
        format_resource_as_label
      )
    ) |>
    dplyr::select(week, date, unit, type, title) |>
    tidyr::pivot_wider(
      names_from = c(unit, type),
      values_from = title
    ) |>
    dplyr::mutate(
      row_group = dplyr::case_when(
        is.na(part_summary) ~ week_summary,
        is.na(week_summary) ~ part_summary,
        .default = paste0(
          part_summary,
          '<div><br></div>',
          week_summary
        )
      )
    ) |>
    dplyr::select(week, date, row_group)

  titles <- schedule |>
    dplyr::filter(
      dplyr::when_any(
        unit == "lecture" & type == "slides",
        unit == "discussion" & type == "activity",
        unit %in% c("potw", "exam")
      )
    ) |>
    dplyr::mutate(
      title = dplyr::case_when(
        unit == "discussion" ~ purrr::map_chr(resource, get_id_title),
        unit == "lecture" ~ purrr::map_chr(resource, get_id_title),
        unit == "potw" ~ format_potw_link(id, resource),
        unit == "exam" ~ format_exam_title_with_buttons(id, resource)
      ),
      title = glue::glue('<div class="unit-id-title">{title}</div>')
    ) |>
    dplyr::select(id, title)

  units <- schedule |>
    dplyr::select(
      week,
      date,
      day,
      show_week,
      current_week,
      id,
      unit,
      type,
      resource
    ) |>
    dplyr::mutate(
      resource = purrr::pmap_chr(
        list(show_week, id, type, resource),
        format_resource_as_label
      ),
      type = fct_to_snake(type)
    ) |>
    tidyr::pivot_wider(
      names_from = type,
      values_from = resource
    ) |>
    dplyr::select(!c(summary, prairielearn))

  unit_schedule <- units |>
    dplyr::left_join(
      row_groups,
      by = dplyr::join_by(week, date),
      relationship = "many-to-one"
    ) |>
    dplyr::arrange(date, unit) |>
    tidyr::fill(row_group) |>
    # maybe move to render_unit_schedule_by_week()
    dplyr::group_by(week) |>
    dplyr::arrange(unit, date, .by_group = TRUE) |>
    dplyr::ungroup() |>
    dplyr::inner_join(
      titles,
      by = dplyr::join_by(id),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      day = day |>
        as.character() |>
        dplyr::replace_when(
          unit == "discussion" & day == "Thu" ~ "1^st^ slot",
          unit == "discussion" & day == "Fri" ~ "2^nd^ slot"
        ),
      date = dplyr::if_else(
        unit == "discussion",
        "",
        gt::vec_fmt_date(date, date_style = "MMMd")
      ),
      date = dplyr::if_else(
        unit %in% c("potw", "exam"),
        format_day_and_date_due(day, date),
        format_day_and_date(day, date)
      ),
      dplyr::mutate(
        dplyr::across(
          tidyselect::where(is.character),
          ~ highlight_current_week(current_week, .x)
        )
      )
    ) |>
    dplyr::filter(show_week) |>
    dplyr::select(
      row_group,
      date,
      title,
      tidyselect::ends_with("lesson_plan"),
      pre_activity,
      slides,
      activity,
      recording
    )

  unit_schedule |>
    gt::gt(
      groupname_col = "row_group",
      process_md = TRUE
    ) |>
    gt::sub_missing(
      missing_text = ""
    ) |>
    gt::cols_label(
      date = "",
      title = "",
      tidyselect::ends_with("lesson_plan") ~ "Lesson plan",
      tidyselect::ends_with("activity") ~ "Activity",
      tidyselect::ends_with("pre_activity") ~ "Pre-activity",
      tidyselect::ends_with("slides") ~ "Slides",
      tidyselect::ends_with("recording") ~ "Video"
    ) |>
    gt::cols_align(
      align = "left",
      columns = c(title)
    ) |>
    gt::cols_align(
      align = "right",
      columns = c(date)
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")
      ),
      locations = list(
        gt::cells_column_labels()
      )
    ) |>
    gt::fmt_markdown() |>
    gt::tab_options(
      quarto.disable_processing = TRUE,
      table.width = "100%"
    )
}
