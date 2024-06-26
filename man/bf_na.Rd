% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bf_na.R
\name{bf_na}
\alias{bf_na}
\title{Build a bit flag by checking whether values are NA}
\usage{
bf_na(
  x,
  test,
  pos = NULL,
  na.val = NULL,
  description = NULL,
  prov = NULL,
  registry = NULL
)
}
\arguments{
\item{x}{\code{\link[=data.frame]{data.frame(1)}}\cr the table that contains
\code{test}.}

\item{test}{\code{\link[=character]{character(1)}}\cr the column in \code{x} that is
checked for NA values.}

\item{pos}{\code{\link[=integer]{integerish(.)}}\cr the position(s) in the bitfield that
should be set.}

\item{na.val}{description}

\item{description}{description}

\item{prov}{description}

\item{registry}{description}
}
\description{
Build a bit flag by checking whether values are NA
}
\examples{
bf_na(x = bityield, test = "y")
}
