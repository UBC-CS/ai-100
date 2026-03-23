source(here::here("R", "get-schedule.R"))
source(here::here("R", "convert-to-title-link.R"))

render_schedule <- function() {
  get_schedule() |>
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
        show_exam | (show_week),
        exam_practice,
        NA
      ),
      # Remove any resources from future weeks
      dplyr::across(
        c(
          dplyr::starts_with(c("tue", "thu")),
          dplyr::ends_with("studio"),
          potw
        ),
        \(column) dplyr::if_else(show_week, column, NA)
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
    gt::fmt_url(
      columns = dplyr::ends_with("slides"),
      label = fontawesome::fa("window-maximize")
    ) |>
    gt::sub_missing(
      columns = dplyr::ends_with("slides"),
      missing_text = fontawesome::fa("window-maximize", fill_opacity = 0.1)
    ) |>
    gt::fmt_url(
      columns = dplyr::ends_with("activities"),
      label = fontawesome::fa("file-alt")
    ) |>
    gt::sub_missing(
      columns = dplyr::ends_with("activities"),
      missing_text = fontawesome::fa("file-alt", fill_opacity = 0.1)
    ) |>
    gt::fmt_url(
      columns = dplyr::ends_with("pre_activities"),
      label = fontawesome::fa("book")
    ) |>
    gt::sub_missing(
      columns = dplyr::ends_with("pre_activities"),
      missing_text = fontawesome::fa("book", fill_opacity = 0.1)
    ) |>
    gt::fmt_url(
      columns = dplyr::ends_with("recording"),
      label = fontawesome::fa("circle-play")
    ) |>
    gt::sub_missing(
      columns = dplyr::ends_with("recording"),
      missing_text = fontawesome::fa("circle-play", fill_opacity = 0.1)
    ) |>
    gt::fmt_url(
      columns = dplyr::ends_with("studio"),
      label = fontawesome::fa("laptop-code")
    ) |>
    gt::sub_missing(
      columns = dplyr::ends_with("studio"),
      missing_text = fontawesome::fa("laptop-code", fill_opacity = 0.1)
    ) |>
    gt::fmt_url(
      columns = potw,
      label = fontawesome::fa("calendar-week")
    ) |>
    gt::sub_missing(
      columns = potw,
      missing_text = fontawesome::fa("calendar-week", fill_opacity = 0.1)
    ) |>
    # fmt_url(
    #   columns = project,
    #   label = fa("list-check")
    # ) |>
    # sub_missing(
    #   columns = project,
    #   missing_text = fa("clipboard-list", fill_opacity = 0.1)
    # ) |>
    # fmt_date(
    #   project_due,
    #   date_style = "MMMd"
    # ) |>
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
      tue_class_pre_activities = "P",
      tue_class_slides = "S",
      tue_class_activities = "A",
      tue_class_recording = "V",
      thu_class_pre_activities = "P",
      thu_class_slides = "S",
      thu_class_activities = "A",
      thu_class_recording = "V",
      thu_studio = gt::md("1^st^"),
      fri_studio = gt::md("2^nd^"),
      potw = "POTW",
      # project = "Guide",
      # project_due = "Due",
      exam = "Book",
      exam_due = "Due",
      exam_practice = "Practice"
    ) |>
    gt::tab_spanner(
      label = "Tue",
      columns = dplyr::starts_with("tue_class"),
      id = "tue_class"
    ) |>
    gt::tab_spanner(
      label = "Thu",
      columns = dplyr::starts_with("thu_class"),
      id = "thu_class"
    ) |>
    gt::tab_spanner(
      label = "Classes",
      spanners = c("tue_class", "thu_class")
    ) |>
    gt::tab_spanner(
      label = "Studios",
      columns = dplyr::ends_with("studio")
    ) |>
    # tab_spanner(
    #   label = "Project",
    #   columns = starts_with("project")
    # ) |>
    gt::tab_spanner(
      label = "Examlet",
      columns = c(exam, exam_due, exam_practice)
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
