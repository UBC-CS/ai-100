source(here::here("R", "convert-to-date.R"))
source(here::here("R", "check-if-student-profile.R"))

get_schedule <- function() {
  rendering_student_profile <- check_if_student_profile()
  current_date <- lubridate::today()

  monday_of_first_term_week <- yaml::read_yaml(
    "_variables.yml"
  )$course$`monday-of-first-term-week`

  # Used to sort column names when using `pivot_wider()`
  sorted_types <- c(
    "summaries",
    "pre_activities",
    "slides",
    "activities",
    "recording",
    "link",
    "practice"
  )
  sorted_units <- c("part", "summary", "class", "studio", "potw", "exam")
  days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  single_resource_units <- c("part", "summary", "potw")

  # Generate ids for each link to join into schedule
  resources_paths <-
    c(
      fs::path("pre-activities"),
      fs::path("activities"),
      fs::path("slides"),
      fs::path("summaries")
    )

  schedule <- readr::read_csv(
    here::here("data", "schedule.csv"),
    col_types = "icc"
  )
  additional_resources <- readr::read_csv(
    here::here("data", "additional-resources.csv"),
    col_types = "ccc"
  )

  resources <-
    tibble::tibble(
      resource = fs::dir_ls(resources_paths, glob = "*.qmd"),
      id = fs::path_file(resource) |> stringr::str_extract("^[^_]+"),
      type = resource |>
        fs::path_dir() |>
        stringr::str_replace("-", "_")
    ) |>
    # Remove unassigned resources indicated by "tbd"
    dplyr::filter(!stringr::str_detect(resource, "tbd")) |>
    dplyr::relocate(resource, .after = type)

  all_resources <- dplyr::bind_rows(resources, additional_resources) |>
    dplyr::mutate(
      type = forcats::fct(type, levels = intersect(sorted_types, type))
    ) |>
    dplyr::arrange(type)

  detailed_schedule <- schedule |>
    dplyr::mutate(
      date = convert_to_date(monday_of_first_term_week, week, day),
      monday = lubridate::floor_date(date, unit = "week", week_start = "Mon"),
      current_week = is_current_week(date, current_date),
      show_week = dplyr::case_when(
        !rendering_student_profile ~ TRUE,
        week == 1 ~ TRUE,
        !is_future_week(date, current_date) ~ TRUE,
        .default = FALSE
      ),
      day = day |>
        stringr::str_to_lower() |>
        forcats::fct(
          levels = intersect(
            stringr::str_to_lower(days),
            stringr::str_to_lower(day)
          )
        ),
      unit = id |> stringr::str_extract("^[^-]+"),
      # Only use levels present in data
      unit = unit |> forcats::fct(levels = intersect(sorted_units, unit)),
      next_exam = dplyr::if_else(unit == "exam", date, NA),
      show_exam = dplyr::between(
        next_exam,
        current_date,
        current_date + lubridate::days(13)
      ),
      .after = date
    ) |>
    # `arrange()` ensures that fill()` propagates `next_exam` to prior dates
    dplyr::arrange(date, unit) |>
    tidyr::fill(next_exam, show_exam, .direction = "up") |>
    dplyr::left_join(
      all_resources,
      by = dplyr::join_by(id),
      relationship = "one-to-many"
    )

  weeks <- detailed_schedule |>
    dplyr::distinct(week, monday, current_week, show_week, show_exam)

  classes <- detailed_schedule |>
    dplyr::filter(unit == "class", !is.na(resource)) |>
    # Ensure the column order after pivoting follows day order
    dplyr::arrange(day) |>
    dplyr::select(week, day, unit, type, resource) |>
    tidyr::pivot_wider(
      names_from = c(day, unit, type),
      names_sep = "_",
      values_from = resource
    )

  studios <- detailed_schedule |>
    dplyr::filter(unit == "studio", !is.na(resource)) |>
    # Ensure the column order after pivoting follows day order
    dplyr::arrange(day) |>
    dplyr::select(week, day, unit, resource) |>
    tidyr::pivot_wider(
      names_from = c(day, unit),
      names_sep = "_",
      values_from = resource
    )

  # Units with only a single resource
  parts_and_other_units <- detailed_schedule |>
    dplyr::filter(unit %in% single_resource_units, !is.na(resource)) |>
    dplyr::select(week, unit, resource) |>
    tidyr::pivot_wider(names_from = unit, values_from = resource) |>
    tidyr::fill(part)

  exams <- detailed_schedule |>
    dplyr::filter(unit == "exam", !is.na(resource)) |>
    dplyr::mutate(
      exam = id |>
        stringr::str_replace("-0{0,1}", " ") |>
        stringr::str_to_title()
    ) |>
    dplyr::select(week, exam, exam_due = date, exam_practice = resource)

  weekly_schedule <- weeks |>
    dplyr::left_join(
      classes,
      by = dplyr::join_by(week),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      studios,
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

  weekly_schedule
}

is_current_week <- function(date, current_date) {
  lubridate::floor_date(date, unit = "week", week_start = "Mon") ==
    lubridate::floor_date(current_date, unit = "week", week_start = "Mon")
}

is_future_week <- function(date, current_date) {
  lubridate::isoweek(date) > lubridate::isoweek(current_date)
}
