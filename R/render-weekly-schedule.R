source(here::here("R", "get-schedule.R"))
source(here::here("R", "convert-to-title-link.R"))
source(here::here("R", "fmt-url-as-icon.R"))

render_weekly_schedule <- function() {
  single_resource_units <- c("part", "summary", "potw")

  schedule <- get_schedule()

  weeks <- schedule |>
    dplyr::distinct(week, monday, current_week, show_week, show_exam)

  classes_and_studios <- schedule |>
    dplyr::filter(unit %in% c("class", "studio"), !is.na(resource)) |>
    # Ensure the column order after pivoting follows day order
    dplyr::arrange(day) |>
    dplyr::select(week, day, unit, type, resource) |>
    tidyr::pivot_wider(
      names_from = c(day, unit, type),
      names_sep = "_",
      values_from = resource
    )

  # Units with only a single resource
  parts_and_other_units <- schedule |>
    dplyr::filter(unit %in% single_resource_units, !is.na(resource)) |>
    dplyr::select(week, unit, resource) |>
    tidyr::pivot_wider(names_from = unit, values_from = resource) |>
    tidyr::fill(part)

  exams <- schedule |>
    dplyr::filter(unit == "exam", !is.na(resource)) |>
    dplyr::mutate(
      exam = id |>
        stringr::str_replace("-0{0,1}", " ") |>
        stringr::str_to_title()
    ) |>
    dplyr::select(week, exam, exam_due = date, exam_practice = resource)

  weekly_schedule <- weeks |>
    dplyr::left_join(
      classes_and_studios,
      by = dplyr::join_by(week),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      parts_and_other_units,
      by = dplyr::join_by(week),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      exams,
      by = dplyr::join_by(week),
      relationship = "one-to-one"
    ) |>
    dplyr::relocate(part)

  weekly_schedule |>
    dplyr::mutate(
      part = convert_to_title_link(part),
      week = dplyr::if_else(
        !is.na(summary),
        glue::glue("[{week} {fontawesome::fa('circle-info')}]({summary})"),
        as.character(week)
      ),
      exam = dplyr::if_else(
        show_exam,
        glue::glue("[{exam}](https://us.prairietest.com)", .na = NULL),
        exam
      ),
      exam_practice = dplyr::if_else(
        show_exam | show_week,
        exam_practice,
        NA
      )
    ) |>
    gt::gt(
      groupname_col = "part",
      process_md = TRUE
    ) |>
    gt::fmt_url(
      columns = week,
      rows = !is.na(summary),
      show_underline = FALSE
    ) |>
    gt::fmt_date(
      monday,
      date_style = "MMMd"
    ) |>
    gt::sub_missing(
      missing_text = ""
    ) |>
    fmt_url_as_icon() |>
    gt::fmt_url(
      columns = exam,
      rows = show_exam,
      show_underline = FALSE
    ) |>
    gt::fmt_date(
      exam_due,
      date_style = "MMMd"
    ) |>
    gt::fmt_url(
      columns = exam_practice,
      label = fontawesome::fa("pen-to-square")
    ) |>
    gt::cols_label(
      week = "Week",
      monday = "Mon",
      tidyselect::ends_with("lesson_plans") ~ "L",
      tidyselect::ends_with("activities") ~ "A",
      tidyselect::ends_with("pre_activities") ~ "P",
      tidyselect::ends_with("slides") ~ "S",
      tidyselect::ends_with("recording") ~ "V",
      potw = "POTW",
      # project = "Guide",
      # project_due = "Due",
      exam = "Book",
      exam_due = "Due",
      exam_practice = "Practice"
    ) |>
    gt::tab_spanner(
      label = "Tue",
      columns = tidyselect::starts_with("tue_class"),
      id = "tue_class"
    ) |>
    gt::tab_spanner(
      label = "Thu",
      columns = tidyselect::starts_with("thu_class"),
      id = "thu_class"
    ) |>
    gt::tab_spanner(
      label = "Classes",
      spanners = c("tue_class", "thu_class")
    ) |>
    gt::tab_spanner(
      label = gt::md("1^st^"),
      columns = tidyselect::starts_with("thu_studio"),
      id = "first_studio"
    ) |>
    gt::tab_spanner(
      label = gt::md("2^nd^"),
      columns = tidyselect::starts_with("fri_studio"),
      id = "second_studio"
    ) |>
    gt::tab_spanner(
      label = "Studios",
      spanners = c("first_studio", "second_studio")
    ) |>
    gt::tab_spanner(
      label = "Project",
      columns = starts_with("project")
    ) |>
    gt::tab_spanner(
      label = "Examlet",
      columns = starts_with("exam")
    ) |>
    gt::cols_align(
      align = "center",
      columns = c(potw, exam_practice)
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
    gtExtras::gt_highlight_rows(
      row = current_week,
      fill = "#ccefff"
    ) |>
    gt::cols_hide(
      c(
        summary,
        current_week,
        show_week,
        show_exam
      )
    ) |>
    gt::tab_options(
      quarto.disable_processing = TRUE,
      table.width = "100%"
    ) |>
    gt::opt_css(
      css = "
        /* Styling for hyperlinks in row group headings */
        .gt_group_heading a {
          color: #008B8B;
          font-weight: bold;
          text-decoration: none;
        }
      "
    )
}
