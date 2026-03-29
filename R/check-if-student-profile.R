check_if_student_profile <- function() {
  quarto_profile <- Sys.getenv("QUARTO_PROFILE")
  student_profiles <- c("student", "student,access")

  quarto_profile %in% student_profiles
}
