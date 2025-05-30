% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz.R
\name{.onUnload}
\alias{.onUnload}
\title{.onUnload hook for package detachment}
\usage{
.onUnload(libpath)
}
\value{
\code{NULL} Cleans up the package on unload
}
\description{
Called when the package is detached.
}
\note{
The box imports no longer work after the package is detached.
}
\keyword{internal}
