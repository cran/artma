% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{options.fix}
\alias{options.fix}
\title{Fix user options file}
\usage{
options.fix(
  options_file_name = NULL,
  options_dir = NULL,
  template_path = NULL,
  force_default_overwrites = TRUE
)
}
\arguments{
\item{options_file_name}{\emph{[character, optional]} Name of the options file to fix, including the .yaml suffix. Defaults to \code{NULL}.}

\item{options_dir}{\emph{[character, optional]} Path to the folder in which to look for user options files. Defaults to \code{NULL}.}

\item{template_path}{\emph{[character, optional]} Path to the options template file. Defaults to \code{NULL}.}

\item{force_default_overwrites}{\emph{[logical, optional]} If set to TRUE, the function will overwrite the existing options file with the default values. Defaults to TRUE.}
}
\value{
\code{NULL} Fixes the user options file.
}
\description{
Fix a user options file by setting the default values for missing options.
}
\details{
The function will attempt to load the user options file and validate it. If any errors are found, the function will attempt to fix them by setting the default values for the missing options.
}
