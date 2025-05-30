% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{options.load}
\alias{options.load}
\title{Load user options}
\usage{
options.load(
  options_file_name = NULL,
  options_dir = NULL,
  create_options_if_null = TRUE,
  load_with_prefix = TRUE,
  template_path = NULL,
  should_validate = TRUE,
  should_set_to_namespace = FALSE,
  should_add_temp_options = FALSE,
  should_return = TRUE
)
}
\arguments{
\item{options_file_name}{\emph{[character, optional]} Name of the options to load, including the .yaml suffix. Defaults to \code{NULL}.}

\item{options_dir}{\emph{[character, optional]} Path to the folder in which to look for user options files. Defaults to \code{NULL}.}

\item{create_options_if_null}{\emph{[logical, optional]} If set to TRUE and the options file name is set to NULL, the function will prompt the user to create a new options file. Defaults to TRUE.}

\item{load_with_prefix}{\emph{[logical, optional]} Whether the options should be loaded with the package prefix. Defaults to TRUE.}

\item{template_path}{\emph{[character, optional]} Path to the template YAML file. Defaults to \code{NULL}.}

\item{should_validate}{\emph{[logical, optional]} Whether the options should be validated after loading. Defaults to TRUE.}

\item{should_set_to_namespace}{\emph{[logical, optional]} Whether the options should be set in the options() namespace. Defaults to TRUE.}

\item{should_add_temp_options}{\emph{[logical, optional]} Whether the options should be added to the temporary options. Defaults to FALSE.}

\item{should_return}{\emph{[logical, optional]} Whether the function should return the list of options. Defaults to FALSE.}
}
\value{
\emph{[list|NULL]} The loaded options as a list or \code{NULL}.
}
\description{
Load user options by their name and return them as a list.
}
\details{
In case the options name is not passed, the function will attempt to load the current options configuration. If none is found, it will then attempt to load the default options. If that fails too, an error is raised.
}
