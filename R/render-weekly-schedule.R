source(here::here("R", "get-schedule.R"))
source(here::here("R", "convert-to-title-link.R"))
source(here::here("R", "fmt-url-as-icon.R"))
source(here::here("R", "highlight-current-week.R"))

render_weekly_schedule <- function() {
  days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  schedule <- get_schedule() |>
    dplyr::mutate(
      day = day |>
        stringr::str_to_lower() |>
        forcats::fct(
          levels = intersect(
            stringr::str_to_lower(days),
            stringr::str_to_lower(day)
          )
        )
    )

  weeks <- schedule |>
    dplyr::distinct(week, monday, current_week, show_week, show_exam)

  lectures_and_discussions <- schedule |>
    dplyr::filter(unit %in% c("lecture", "discussion"), !is.na(resource)) |>
    # Ensure the column order after pivoting follows day order
    dplyr::arrange(day) |>
    dplyr::select(week, day, unit, type, resource) |>
    tidyr::pivot_wider(
      names_from = c(day, unit, type),
      names_sep = "_",
      values_from = resource
    )

  # Units with only a single resource
  parts_and_weeks <- schedule |>
    dplyr::filter(unit %in% c("part", "week"), !is.na(resource)) |>
    dplyr::select(week, unit, type, resource) |>
    tidyr::pivot_wider(
      names_from = c(unit, type),
      values_from = resource
    ) |>
    tidyr::fill(part_summaries)

  potw <- schedule |>
    dplyr::filter(unit == "potw", !is.na(resource)) |>
    dplyr::select(week, potw = resource)

  exams <- schedule |>
    dplyr::filter(unit == "exam", !is.na(resource)) |>
    dplyr::mutate(
      exam = id |>
        stringr::str_replace("-0{0,1}", " ") |>
        stringr::str_to_title(),
      day = day |> stringr::str_to_title()
    ) |>
    dplyr::select(
      week,
      exam,
      exam_day = day,
      exam_due = date,
      exam_practice = resource
    )

  weekly_schedule <- weeks |>
    dplyr::left_join(
      lectures_and_discussions,
      by = dplyr::join_by(week),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      parts_and_weeks,
      by = dplyr::join_by(week),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      potw,
      by = dplyr::join_by(week),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      exams,
      by = dplyr::join_by(week),
      relationship = "one-to-one"
    ) |>
    dplyr::relocate(part_summaries)

  weekly_schedule |>
    dplyr::mutate(
      part_summaries = convert_to_title_link(part_summaries),
      week = glue::glue("Week {week}") |>
        (\(week_title) {
          dplyr::if_else(
            !is.na(week_summaries),
            glue::glue(
              "[{week_title}]({week_summaries})"
            ),
            week_title
          )
        })()
    ) |>
    gt::gt(
      groupname_col = "part_summaries",
      process_md = TRUE
    ) |>
    gt::fmt_url(
      columns = week,
      rows = !is.na(week_summaries),
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
    gt::fmt_date(
      exam_due,
      date_style = "MMMd"
    ) |>
    gt::cols_add(
      after_week_spacer = '<span class="spacer"></span>',
      .after = "week"
    ) |>
    gt::cols_add(
      lecture_half_spacer = '<span class="half-spacer"></span>',
      .after = "tue_lecture_recording"
    ) |>
    gt::cols_add(
      between_spanners_spacer = '<span class="half-spacer"></span>',
      .after = "thu_lecture_recording"
    ) |>
    gt::cols_add(
      discussion_half_spacer = '<span class="half-spacer"></span>',
      .after = "thu_discussion_activities"
    ) |>
    gt::cols_add(
      before_potw_spacer = '<span class="half-spacer"></span>',
      .before = "potw"
    ) |>
    gt::cols_add(
      after_potw_spacer = '<span class="half-spacer"></span>',
      .after = "potw"
    ) |>
    gt::fmt_markdown(columns = tidyselect::contains("spacer")) |>
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
      align = "center",
      columns = c(potw)
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
    gt::cols_merge(
      columns = c(week, monday),
      pattern = paste0(
        '<span class="monday-and-week">',
        '{1}\n',
        '<span class="monday-of-week">',
        'starts {2}',
        '</span>',
        '</span>'
      )
    ) |>
    gt::cols_merge(
      columns = c(exam, exam_day, exam_due),
      rows = !is.na(exam),
      pattern = paste0(
        '<span class="due-date-and-exam">',
        '{1}\n',
        '<span class="day-and-date">',
        'Due: {2} {3}',
        '</span>',
        '</span>'
      )
    ) |>
    gt::cols_merge(
      columns = c(exam, exam_practice),
      rows = (!is.na(exam_day) & (show_exam | show_week)),
      pattern = paste0(
        '<span class="exam-booking">',
        '{1}\n',
        '<a href="https://us.prairietest.com" class="exam-button">Book</a>',
        ' ',
        '<a href="{2}"class="exam-button">Practice</a>',
        '</span>'
      )
    ) |>
    gt::fmt_markdown(
      columns = exam_day,
      rows = show_exam
    ) |>
    highlight_current_week() |>
    gt::cols_hide(
      c(
        week_summaries,
        current_week,
        show_week,
        show_exam
      )
    ) |>
    gt::tab_options(
      quarto.disable_processing = TRUE,
      table.width = "100%"
    )
}
