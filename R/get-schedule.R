source(here::here("R", "convert-to-date.R"))

get_schedule <- function() {
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
  sorted_units <- c("summary", "class", "potw", "studio", "exam")
  days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  single_resource_units <- c("summary", "potw")

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
  parts <- readr::read_csv(
    here::here("data", "parts.csv"),
    col_types = "ic"
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
      current_week = lubridate::isoweek(date) ==
        lubridate::isoweek(current_date),
      show_week = TRUE,
      # show_week = lubridate::isoweek(current_date) >= lubridate::isoweek(date),
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
      parts,
      by = dplyr::join_by(dplyr::closest(week >= start_week)),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      all_resources,
      by = dplyr::join_by(id),
      relationship = "one-to-many"
    )

  weeks <- detailed_schedule |>
    dplyr::distinct(part, week, monday, current_week, show_week, show_exam)

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
  other_units <- detailed_schedule |>
    dplyr::filter(unit %in% single_resource_units, !is.na(resource)) |>
    dplyr::select(week, unit, resource) |>
    tidyr::pivot_wider(names_from = unit, values_from = resource)

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
      other_units,
      by = dplyr::join_by(week),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      exams,
      by = dplyr::join_by(week),
      relationship = "one-to-one"
    )

  weekly_schedule
}
