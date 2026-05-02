# For nix-R session, remove `R_LIBS_USER`, system's R user library.`.
# This guarantees no user libraries from the system are loaded and only
# R packages in the Nix store are used. This makes Nix-R behave in pure manner
# at run-time.

is_nix_r <- nzchar(Sys.getenv("NIX_STORE"))
if (isTRUE(is_nix_r)) {
  install.packages <- function(...) {
    stop(
      "You are currently in an R session running from Nix.\n",
      "Don't install packages using install.packages(),\nadd them to ",
      "the flake.nix file instead."
    )
  }
  update.packages <- function(...) {
    stop(
      "You are currently in an R session running from Nix.\n",
      "Don't update packages using update.packages(),",
      "update flake.nix with a more recent version of R."
    )
  }
  remove.packages <- function(...) {
    stop(
      "You are currently in an R session running from Nix.\n",
      "Don't remove packages using `remove.packages()``,\ndelete them ",
      "from the flake.nix file instead."
    )
  }
  current_paths <- .libPaths()
  userlib_paths <- Sys.getenv("R_LIBS_USER")
  user_dir <- grep(
    paste(userlib_paths, collapse = "|"),
    current_paths,
    fixed = TRUE
  )
  new_paths <- current_paths[-user_dir]
  .libPaths(new_paths)
  rm(current_paths, userlib_paths, user_dir, new_paths)
}
rm(is_nix_r)

# Set error handler to rlang
if (require(rlang, quietly = TRUE)) {
  globalCallingHandlers(error = rlang::entrace)
}
