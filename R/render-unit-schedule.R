source(here::here("R", "convert-to-title.R"))
source(here::here("R", "convert-to-title-link.R"))
source(here::here("R", "fmt-url-as-icon.R"))
source(here::here("R", "get-schedule.R"))
source(here::here("R", "highlight-current-week.R"))

render_unit_schedule <- function() {
  schedule <- get_schedule() |>
    dplyr::mutate(
      day = dplyr::replace_when(
        day,
        unit == "discussion" & day == "Thu" ~ "1^st^ slot",
        unit == "discussion" & day == "Fri" ~ "2^nd^ slot",
        unit %in% c("potw", "exam") ~ paste0("Due: ", day)
      )
    )

  parts_and_weeks <- schedule |>
    dplyr::filter(type == "summary", !is.na(resource)) |>
    dplyr::select(week, date, unit, type, resource) |>
    tidyr::pivot_wider(
      names_from = c(unit, type),
      values_from = resource
    )

  units <- schedule |>
    dplyr::filter(
      unit %in% c("lecture", "discussion", "potw", "exam"),
      !is.na(resource)
    ) |>
    dplyr::select(
      week,
      date,
      show_week,
      current_week,
      show_exam,
      day,
      id,
      unit,
      type,
      resource
    ) |>
    dplyr::mutate(type = type |> as.character() |> stringr::str_to_snake()) |>
    tidyr::pivot_wider(
      names_from = type,
      values_from = resource
    )

  unit_schedule <- parts_and_weeks |>
    dplyr::full_join(
      units,
      by = dplyr::join_by(week, date),
      relationship = "one-to-many"
    ) |>
    dplyr::arrange(date, unit) |>
    tidyr::fill(part_summary, week_summary) |>
    # maybe move to render_unit_schedule_by_week()
    dplyr::group_by(week) |>
    dplyr::arrange(unit, date, .by_group = TRUE) |>
    dplyr::ungroup() |>
    dplyr::filter(
      dplyr::when_any(show_week, show_exam),
      !is.na(unit)
    ) |>
    dplyr::mutate(
      week_summary = dplyr::if_else(
        !is.na(week_summary),
        glue::glue(
          "[Week {week}]({week_summary})"
        ),
        glue::glue("Week {week}")
      ),
      part_summary = convert_to_title_link(part_summary),
      date = dplyr::if_else(unit == "discussion", NA, date),
      title = dplyr::case_when(
        unit == "discussion" ~ convert_to_title(activity),
        unit == "lecture" ~ convert_to_title(slides),
        unit == "potw" ~ glue::glue(
          '<span class="unit-full-title">',
          '<span class="placeholder">Placeholder</span>\n',
          '[Problems of the week]({prairielearn})',
          '</span>'
        ),
        unit == "exam" ~ id |>
          stringr::str_replace("-0{0,1}", " ") |>
          stringr::str_to_title()
      )
    )

  unit_schedule_by_row_group <- unit_schedule |>
    dplyr::mutate(
      consecutive_part = dplyr::consecutive_id(part_summary),
      consecutive_week = dplyr::consecutive_id(week_summary),
      consecutive_combined = dplyr::consecutive_id(
        part_summary,
        week_summary
      )
    ) |>
    dplyr::mutate(
      row_group_part = dplyr::if_else(
        dplyr::row_number() == 1,
        part_summary,
        NA
      ),
      .by = consecutive_part
    ) |>
    dplyr::mutate(
      row_group_week = dplyr::if_else(
        dplyr::row_number() == 1,
        week_summary,
        NA
      ),
      .by = consecutive_week
    ) |>
    tidyr::fill(
      row_group_part,
      row_group_week,
      .by = consecutive_combined
    ) |>
    dplyr::mutate(
      row_group = dplyr::case_when(
        is.na(row_group_part) ~ row_group_week,
        is.na(row_group_week) ~ row_group_part,
        .default = paste0(
          '<span class="part-and-week">',
          row_group_part,
          '\n\n',
          row_group_week,
          '</span>'
        )
      )
    ) |>
    dplyr::select(
      row_group,
      show_week,
      current_week,
      date,
      day,
      unit,
      title,
      tidyselect::ends_with("lesson_plans"),
      pre_activity,
      slides,
      activity,
      recording,
      prairielearn
    )

  unit_schedule_by_row_group |>
    gt::gt(
      groupname_col = "row_group",
      process_md = TRUE
    ) |>
    gt::fmt_markdown(
      columns = c(day, title),
    ) |>
    gt::fmt_date(
      date,
      date_style = "MMMd"
    ) |>
    gt::sub_missing(
      missing_text = ""
    ) |>
    fmt_url_as_icon() |>
    gt::cols_label(
      date = "",
      day = "",
      title = "",
      tidyselect::ends_with("lesson_plan") ~ "Lesson plan",
      tidyselect::ends_with("activitiy") ~ "Activity",
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
      columns = c(day)
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")
      ),
      locations = list(
        gt::cells_column_labels()
      )
    ) |>
    gt::cols_merge(
      columns = c(day, date),
      pattern = paste0(
        '<span class="day-and-date-combined">',
        '<span class="day-of-week">',
        '{1}',
        '</span>\n',
        '{2}',
        '</span>'
      )
    ) |>
    gt::text_transform(
      fn = function(x) {
        paste0('<span class="day-and-date-due">', x, '</span>')
      },
      locations = gt::cells_body(
        columns = day,
        rows = (unit == "potw" | unit == "exam")
      )
    ) |>
    gt::tab_style(
      style = "vertical-align: top",
      locations = gt::cells_body(
        columns = c(day, title)
      )
    ) |>
    gt::cols_merge(
      columns = c(title, prairielearn),
      rows = unit == "exam",
      pattern = paste0(
        '<span class="exam-booking">',
        '<span class="placeholder">Placeholder</span>\n',
        '{1} ',
        '<a href="https://us.prairietest.com" class="exam-button">Book</a>',
        ' ',
        '<a href="{2}"class="exam-button">Practice</a>',
        '</span>'
      )
    ) |>
    highlight_current_week() |>
    gt::cols_hide(
      c(
        current_week,
        show_week,
        unit
      )
    ) |>
    gt::tab_options(
      quarto.disable_processing = TRUE,
      table.width = "100%"
    )
}
