source(here::here("R", "convert-to-date.R"))
source(here::here("R", "check-if-student-profile.R"))
source(here::here("R", "get-id-from-resource.R"))

get_schedule <- function() {
  rendering_student_profile <- check_if_student_profile()
  current_date <- lubridate::today()

  lookup <- readr::read_csv(here::here("data", "lookup.csv"), col_types = "cc")

  monday_of_first_term_week <- yaml::read_yaml(
    "_variables.yml"
  )$course$`monday-of-first-term-week`

  # Sequence of `type`s in `lookup` table determines column sequence
  # when using `pivot_wider()`
  sorted_types <- lookup$type

  # Generate ids for each link to join into schedule
  resources_paths <- lookup$directory |> na.omit()

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
      id = get_id_from_resource(resource),
      type = resource |> fs::path_dir()
    ) |>
    dplyr::relocate(resource, .after = type)

  all_resources <- dplyr::bind_rows(resources, additional_resources) |>
    dplyr::mutate(
      # Sequence of `id`s in `schedule.csv` determines column sequence
      # when using `pivot_wider()`
      unit = id |> stringr::str_extract("^[^-]+") |> forcats::fct(),
      type = type |>
        dplyr::replace_values(from = lookup$directory, to = lookup$type),
      type = type |>
        forcats::fct(levels = intersect(sorted_types, type))
    ) |>
    dplyr::arrange(type)

  schedule |>
    dplyr::left_join(
      all_resources,
      by = dplyr::join_by(id),
      relationship = "one-to-many"
    ) |>
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
      next_exam = dplyr::if_else(unit == "exam", date, NA),
      show_exam = dplyr::between(
        next_exam,
        current_date,
        current_date + lubridate::days(13)
      ),
      .after = day
    ) |>
    # `arrange()` ensures that fill()` propagates `next_exam` to prior dates
    dplyr::arrange(date, unit) |>
    tidyr::fill(next_exam, show_exam, .direction = "up") |>
    dplyr::filter_out(
      dplyr::when_all(rendering_student_profile & type == "lesson_plans")
    )
}

is_current_week <- function(date, current_date) {
  lubridate::floor_date(date, unit = "week", week_start = "Mon") ==
    lubridate::floor_date(current_date, unit = "week", week_start = "Mon")
}

is_future_week <- function(date, current_date) {
  lubridate::isoweek(date) > lubridate::isoweek(current_date)
}
