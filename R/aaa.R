# nolint start: unused_declared_object_linter, box_usage_linter.

#' @title Runtime Setup
#' @description
#' A function user as a wrapper for runtime functions invocation to ensure crucial fucntionality, such as imports, etc., all work as expected.
#'
#' @param FUN [function] The function to be called after the setup.
#' @param options_file_name *\[character\]* Name of the options file to use, including the suffix.
#' @param options_dir *\[character, optional\]* Path to the directory that contains user options. Defaults to the directory specified in PATHS.
#' @keywords internal
runtime_setup <- function(
    FUN,
    options_file_name = NULL,
    options_dir = NULL) {
  if (is.null(options_file_name) && !interactive()) {
    cli::cli_alert_warning("Running in non-interactive mode without providing an options file name. Please provide an options file name or run in interactive mode.")
    return(invisible())
  }

  runtime_options <- options.load(
    options_file_name = options_file_name,
    options_dir = options_dir,
    should_validate = TRUE,
    should_add_temp_options = TRUE # Load to the options() namespace
  )

  withr::local_options(runtime_options)

  FUN()
}

# nolint end: unused_declared_object_linter, box_usage_linter.
