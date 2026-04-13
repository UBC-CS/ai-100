source(here::here("R", "fct-to-lower.R"))
source(here::here("R", "fct-to-snake.R"))
source(here::here("R", "format-exam-with-due-date.R"))
source(here::here("R", "format-resource-as-label.R"))
source(here::here("R", "format-week-with-start-day.R"))
source(here::here("R", "get-schedule.R"))
source(here::here("R", "highlight-current-week.R"))

render_weekly_schedule <- function() {
  schedule <- get_schedule() |>
    dplyr::filter_out(is.na(type)) |>
    dplyr::mutate(
      date = gt::vec_fmt_date(date, date_style = "MMMd"),
      label = purrr::pmap_chr(
        list(show_week, id, type, resource),
        format_resource_as_label
      ),
      label = dplyr::replace_when(
        label,
        unit == "week" ~ format_week_with_start_day(date, id, resource),
        unit == "exam" ~ format_exam_with_due_date(
          day,
          date,
          show_week,
          show_exam,
          id,
          resource
        )
      )
    )

  weeks <- schedule |>
    dplyr::distinct(week, current_week, show_week, show_exam)

  lectures_and_discussions <- schedule |>
    dplyr::filter(unit %in% c("lecture", "discussion")) |>
    # Ensure the column order after pivoting follows day order
    dplyr::arrange(day) |>
    dplyr::select(
      week_number = week,
      current_week,
      show_week,
      show_exam,
      day,
      unit,
      type,
      label
    ) |>
    dplyr::mutate(
      day = fct_to_lower(day),
      type = fct_to_snake(type)
    ) |>
    tidyr::pivot_wider(
      names_from = c(day, unit, type),
      names_sep = "_",
      values_from = label
    ) |>
    dplyr::arrange(week_number)

  # Units with only a single resource
  other_units <- schedule |>
    dplyr::filter(unit %in% c("part", "week", "potw", "exam")) |>
    dplyr::select(week_number = week, unit, label) |>
    tidyr::pivot_wider(
      names_from = unit,
      values_from = label
    ) |>
    tidyr::fill(part)

  weekly_schedule <- lectures_and_discussions |>
    dplyr::left_join(
      other_units,
      by = dplyr::join_by(week_number),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character),
        ~ highlight_current_week(current_week, .x)
      )
    ) |>
    dplyr::select(!c(week_number, current_week, show_week, show_exam)) |>
    dplyr::relocate(week)

  spacer <- '<span class="spacer"></span>'
  half_spacer <- '<span class="half-spacer"></span>'

  weekly_schedule |>
    gt::gt(
      groupname_col = "part",
      process_md = TRUE
    ) |>
    gt::sub_missing(missing_text = "") |>
    gt::cols_add(
      after_week_spacer = spacer,
      .after = "week"
    ) |>
    gt::cols_add(
      lecture_half_spacer = half_spacer,
      .after = "tue_lecture_recording"
    ) |>
    gt::cols_add(
      between_spanners_spacer = half_spacer,
      .after = "thu_lecture_recording"
    ) |>
    gt::cols_add(
      discussion_half_spacer = half_spacer,
      .after = "thu_discussion_activity"
    ) |>
    gt::cols_add(
      before_potw_spacer = half_spacer,
      .before = "potw"
    ) |>
    gt::cols_add(
      after_potw_spacer = half_spacer,
      .after = "potw"
    ) |>
    gt::cols_label(tidyselect::everything() ~ "") |>
    gt::tab_spanner(
      label = "Tue",
      columns = tidyselect::starts_with("tue_lecture"),
      id = "tue_lecture"
    ) |>
    gt::tab_spanner(
      label = "Thu",
      columns = tidyselect::starts_with("thu_lecture"),
      id = "thu_lecture"
    ) |>
    gt::tab_spanner(
      label = "Lectures",
      columns = tidyselect::contains("lecture"),
      spanners = c("tue_lecture", "thu_lecture")
    ) |>
    gt::tab_spanner(
      label = gt::md("1^st^"),
      columns = tidyselect::starts_with("thu_discussion"),
      id = "first_discussion"
    ) |>
    gt::tab_spanner(
      label = gt::md("2^nd^"),
      columns = tidyselect::starts_with("fri_discussion"),
      id = "second_discussion"
    ) |>
    gt::tab_spanner(
      label = "Discussions",
      columns = tidyselect::contains("discussion"),
      spanners = c("first_discussion", "second_discussion")
    ) |>
    gt::tab_spanner(
      label = "Project",
      columns = starts_with("project")
    ) |>
    gt::tab_style(
      style = gt::cell_text(size = "small"),
      locations = gt::cells_column_spanners()
    ) |>
    gt::cols_align(
      align = "left",
      columns = c(exam)
    ) |>
    gt::cols_align(
      align = "right",
      columns = c(week)
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")
      ),
      locations = list(
        gt::cells_column_labels(),
        gt::cells_column_spanners()
      )
    ) |>
    gt::fmt_markdown() |>
    gt::tab_options(
      quarto.disable_processing = TRUE,
      table.width = "100%"
    )
}
