source(here::here("R", "convert-to-title.R"))
source(here::here("R", "convert-to-title-link.R"))
source(here::here("R", "fmt-url-as-icon.R"))
source(here::here("R", "get-schedule.R"))

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
    dplyr::filter(type == "summaries", !is.na(resource)) |>
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
    tidyr::fill(part_summaries, week_summaries) |>
    # maybe move to render_unit_schedule_by_week()
    dplyr::group_by(week) |>
    dplyr::arrange(unit, date, .by_group = TRUE) |>
    dplyr::ungroup() |>
    dplyr::filter(
      dplyr::when_any(show_week, show_exam),
      !is.na(unit)
    ) |>
    dplyr::mutate(
      week_summaries = dplyr::if_else(
        !is.na(week_summaries),
        glue::glue(
          "[Week {week}]({week_summaries})"
        ),
        glue::glue("Week {week}")
      ),
      part_summaries = convert_to_title_link(part_summaries),
      date = dplyr::if_else(unit == "discussion", NA, date),
      title = dplyr::case_when(
        unit == "discussion" ~ convert_to_title(activities),
        unit == "lecture" ~ convert_to_title(slides),
        unit == "potw" ~ glue::glue("[Problems of the week]({link})"),
        unit == "exam" ~ glue::glue(
          "{id |> stringr::str_replace('-0{0,1}', ' ') |> ",
          "stringr::str_to_title()} ([Book](https://us.prairietest.com), ",
          "[Practice]({practice}))"
        )
      )
    )

  unit_schedule_by_row_group <- unit_schedule |>
    dplyr::mutate(
      consecutive_part = dplyr::consecutive_id(part_summaries),
      consecutive_week = dplyr::consecutive_id(week_summaries),
      consecutive_combined = dplyr::consecutive_id(
        part_summaries,
        week_summaries
      )
    ) |>
    dplyr::mutate(
      row_group_part = dplyr::if_else(
        dplyr::row_number() == 1,
        part_summaries,
        NA
      ),
      .by = consecutive_part
    ) |>
    dplyr::mutate(
      row_group_week = dplyr::if_else(
        dplyr::row_number() == 1,
        week_summaries,
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
      pre_activities,
      slides,
      activities,
      recording
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
      tidyselect::ends_with("lesson_plans") ~ "Lesson plan",
      tidyselect::ends_with("activities") ~ "Activity",
      tidyselect::ends_with("pre_activities") ~ "Pre-activity",
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
    # Colour must be changed before columns merge
    gt::tab_style(
      style = gt::cell_text(color = "#FF0000"),
      locations = gt::cells_body(
        columns = c(date, day),
        rows = (unit == "potw" | unit == "exam")
      )
    ) |>
    gt::cols_merge(
      columns = c(day, date),
      pattern = paste0(
        '<span class="day-and-date">',
        '<span class="day-of-week">',
        '{1}',
        '</span>\n',
        '{2}',
        '</span>'
      )
    ) |>
    gt::tab_style(
      style = "vertical-align:top",
      locations = gt::cells_body(
        columns = day
      )
    ) |>
    gt::tab_style(
      style = "vertical-align:bottom",
      locations = gt::cells_body(
        columns = title
      )
    ) |>
    gtExtras::gt_highlight_rows(
      row = current_week,
      fill = "#ccefff"
    ) |>
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
    ) |>
    gt::opt_css(
      css = "
        /* Styling for hyperlinks in row group headings */
        tr a {
          color: #008B8B;
          font-weight: bold;
          text-decoration: none;
        }

        .unit-full-title, .part-and-week {
          white-space: pre-wrap;
        }

        .unit-title, .day-of-week {
          font-size: smaller;
          font-style: italic;
          opacity: 0.6;
        }

        .day-and-date {
          white-space: pre;
        }
      "
    )
}
