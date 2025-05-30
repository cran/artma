% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{options.modify}
\alias{options.modify}
\title{Modify User Options}
\usage{
options.modify(
  options_file_name = NULL,
  options_dir = NULL,
  template_path = NULL,
  user_input = list(),
  should_validate = TRUE
)
}
\arguments{
\item{options_file_name}{\emph{[character, optional]} Name of the user options file to modify, including the suffix.}

\item{options_dir}{\emph{[character, optional]} Full path to the folder that contains user options files. If not provided, the default folder is chosen. Defaults to \code{NULL}.}

\item{template_path}{\emph{[character, optional]} Full path to the options template file. Defaults to \code{NULL}.}

\item{user_input}{\emph{[list, optional]} A named list of user-supplied values for these options. If \code{NULL} or missing entries exist, the function will prompt the user via \code{readline()} (for required entries) or use defaults (for optional ones).}

\item{should_validate}{\emph{[logical, optional]} If TRUE, validate the modified options file against the template. Defaults to TRUE.}
}
\value{
\code{NULL}
}
\description{
Modify an existing user options file with new values.
}
