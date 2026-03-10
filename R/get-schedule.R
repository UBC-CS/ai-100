library(conflicted)
library(dplyr)
conflicts_prefer(dplyr::filter)
library(forcats)
library(fs)
library(here)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

get_schedule <- function() {
  current_date <- today()

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
      path("pre-activities"),
      path("activities"),
      path("slides"),
      path("summaries")
    )

  schedule <- read_csv(here("data", "schedule.csv"), show_col_types = FALSE)
  sections <- read_csv(here("data", "sections.csv"), show_col_types = FALSE)
  additional_resources <- read_csv(
    here("data", "additional-resources.csv"),
    show_col_types = FALSE
  )

  resources <-
    tibble(
      resource = dir_ls(resources_paths, glob = "*.qmd"),
      id = path_file(resource) |> str_extract("^[^_]+"),
      type = resource |>
        path_dir() |>
        str_replace("-", "_")
    ) |>
    # Remove unassigned resources indicated by "tbd"
    filter(!str_detect(resource, "tbd")) |>
    relocate(resource, .after = type)

  all_resources <- bind_rows(resources, additional_resources) |>
    mutate(type = fct(type, levels = intersect(sorted_types, type))) |>
    arrange(type)

  detailed_schedule <- schedule |>
    mutate(
      week = date |> isoweek() |> consecutive_id(),
      monday = floor_date(date, unit = "week", week_start = "Mon"),
      current_week = isoweek(date) == isoweek(current_date),
      show_week = isoweek(date) >= isoweek(current_date),
      day = wday(date, label = TRUE, week_start = "Mon") |>
        str_to_lower(),
      day = day |>
        fct(levels = intersect(str_to_lower(days), str_to_lower(day))),
      unit = id |> str_extract("^[^-]+"),
      # Only use levels present in data
      unit = unit |> fct(levels = intersect(sorted_units, unit)),
      next_exam = if_else(unit == "exam", date, NA),
      show_exam = between(next_exam, current_date, current_date + days(13)),
      .after = date
    ) |>
    fill(next_exam, show_exam, .direction = "up") |>
    left_join(
      sections,
      by = join_by(closest(date >= start_date)),
      relationship = "many-to-one"
    ) |>
    left_join(
      all_resources,
      by = join_by(id),
      relationship = "one-to-many"
    )

  weeks <- detailed_schedule |>
    distinct(week, monday, current_week, show_week, show_exam)

  classes <- detailed_schedule |>
    filter(unit == "class", !is.na(resource)) |>
    # Ensure the column order after pivoting follows day order
    arrange(day) |>
    select(week, day, unit, type, resource) |>
    pivot_wider(
      names_from = c(day, unit, type),
      names_sep = "_",
      values_from = resource
    )

  studios <- detailed_schedule |>
    filter(unit == "studio", !is.na(resource)) |>
    # Ensure the column order after pivoting follows day order
    arrange(day) |>
    select(week, day, unit, resource) |>
    pivot_wider(
      names_from = c(day, unit),
      names_sep = "_",
      values_from = resource
    )

  # Units with only a single resource
  other_units <- detailed_schedule |>
    filter(unit %in% single_resource_units, !is.na(resource)) |>
    select(week, unit, resource) |>
    pivot_wider(names_from = unit, values_from = resource)

  exams <- detailed_schedule |>
    filter(unit == "exam", !is.na(resource)) |>
    mutate(exam = id |> str_replace("-", " ") |> str_to_title()) |>
    select(week, exam, exam_due = date, exam_practice = resource)

  weekly_schedule <- weeks |>
    left_join(
      classes,
      by = join_by(week),
      relationship = "one-to-one"
    ) |>
    left_join(
      studios,
      by = join_by(week),
      relationship = "one-to-one"
    ) |>
    left_join(
      other_units,
      by = join_by(week),
      relationship = "one-to-one"
    ) |>
    left_join(exams, by = join_by(week), relationship = "one-to-one")

  weekly_schedule
}
