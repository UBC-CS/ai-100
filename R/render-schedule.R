library(conflicted)
library(dplyr)
conflicts_prefer(dplyr::filter)
library(fontawesome)
library(glue)
library(gt)
library(gtExtras)
library(here)

source(here("R", "get-schedule.R"))

render_schedule <- function() {
  get_schedule() |>
    mutate(
      week = if_else(
        !is.na(summary),
        glue("[{week} {fa('circle-info')}]({summary})"),
        as.character(week)
      ),
      exam = if_else(
        show_exam,
        glue("[{exam}](https://us.prairietest.com)", .na = NULL),
        exam
      ),
      exam_practice = if_else(
        show_exam | (show_week),
        exam_practice,
        NA
      ),
      # Remove any resources from future weeks
      across(
        c(starts_with(c("tue", "thu")), lab, potw),
        \(column) if_else(show_week, column, NA)
      )
    ) |>
    gt(
      # groupname_col = "part"
    ) |>
    fmt_url(
      columns = week,
      rows = !is.na(summary),
      show_underline = FALSE
    ) |>
    fmt_date(
      monday,
      date_style = "MMMd"
    ) |>
    sub_missing(
      missing_text = ""
    ) |>
    fmt_url(
      columns = ends_with("slides"),
      label = fa("window-maximize")
    ) |>
    sub_missing(
      columns = ends_with("slides"),
      missing_text = fa("window-maximize", fill_opacity = 0.1)
    ) |>
    fmt_url(
      columns = ends_with("activities"),
      label = fa("file-alt")
    ) |>
    sub_missing(
      columns = ends_with("activities"),
      missing_text = fa("file-alt", fill_opacity = 0.1)
    ) |>
    fmt_url(
      columns = ends_with("pre_activities"),
      label = fa("book")
    ) |>
    sub_missing(
      columns = ends_with("pre_activities"),
      missing_text = fa("book", fill_opacity = 0.1)
    ) |>
    fmt_url(
      columns = ends_with("recording"),
      label = fa("circle-play")
    ) |>
    sub_missing(
      columns = ends_with("recording"),
      missing_text = fa("circle-play", fill_opacity = 0.1)
    ) |>
    fmt_url(
      columns = lab,
      label = fa("laptop-code")
    ) |>
    sub_missing(
      columns = lab,
      missing_text = fa("laptop-code", fill_opacity = 0.1)
    ) |>
    fmt_url(
      columns = potw,
      label = fa("calendar-week")
    ) |>
    sub_missing(
      columns = potw,
      missing_text = fa("calendar-week", fill_opacity = 0.1)
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
    fmt_url(
      columns = exam,
      rows = show_exam,
      show_underline = FALSE
    ) |>
    fmt_date(
      exam_due,
      date_style = "MMMd"
    ) |>
    fmt_url(
      columns = exam_practice,
      label = fa("pen-to-square")
    ) |>
    cols_label(
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
      lab = "Lab",
      potw = "POTW",
      # project = "Guide",
      # project_due = "Due",
      exam = "Book",
      exam_due = "Due",
      exam_practice = "Practice"
    ) |>
    tab_spanner(
      label = "Tue",
      columns = starts_with("tue")
    ) |>
    tab_spanner(
      label = "Thu",
      columns = starts_with("thu")
    ) |>
    # tab_spanner(
    #   label = "Project",
    #   columns = starts_with("project")
    # ) |>
    tab_spanner(
      label = "Examlet",
      columns = c(exam, exam_due, exam_practice)
    ) |>
    cols_align(
      align = "center",
      columns = c(lab, potw, exam_practice)
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = list(
        cells_row_groups(),
        cells_stubhead(),
        cells_column_labels(),
        cells_column_spanners()
      )
    ) |>
    gt_highlight_rows(
      row = current_week,
      fill = "#ccefff"
    ) |>
    cols_hide(
      c(
        summary,
        current_week,
        show_week,
        show_exam
      )
    ) |>
    tab_options(
      quarto.disable_processing = TRUE,
      table.width = "100%"
    )
}
