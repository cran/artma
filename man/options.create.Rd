% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{options.create}
\alias{options.create}
\title{Create user options}
\usage{
options.create(
  options_file_name = NULL,
  options_dir = NULL,
  template_path = NULL,
  user_input = list(),
  should_validate = TRUE,
  should_overwrite = FALSE,
  action_name = "creating"
)
}
\arguments{
\item{options_file_name}{\emph{[character]} Name of the new user options file, including the suffix.}

\item{options_dir}{\emph{[character, optional]} Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to \code{NULL}.}

\item{template_path}{\emph{[character, optional]} Full path to the options template file.}

\item{user_input}{\emph{[list, optional]} A named list of user-supplied values for these options. If \code{NULL} or missing entries exist, the function will prompt the user via \code{readline()} (for required entries) or use defaults (for optional ones).}

\item{should_validate}{\emph{[logical, optional]} If TRUE, validate the new options file against the template. Defaults to TRUE.}

\item{should_overwrite}{\emph{[logical, optional]} If TRUE, overwrite the file if it already exists. Defaults to FALSE, in which case the user is prompted to confirm the overwrite.}

\item{action_name}{\emph{[character, optional]} A name for the action being performed. This is used for logging purposes. Defaults to "create".
\code{character} Name of the newly created user options file as a character.}
}
\value{
\code{NULL}
}
\description{
Create a new user options file from an options template.
}
