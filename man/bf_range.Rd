% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bf_range.R
\name{bf_range}
\alias{bf_range}
\title{Build a bit flag by checking for a range}
\usage{
bf_range(x, test, min = NULL, max = NULL)
}
\arguments{
\item{x}{\code{\link[=data.frame]{data.frame(1)}}\cr the table that contains
\code{test}.}

\item{test}{\code{\link[=character]{character(1)}}\cr the column in \code{x} for which a
range is checked.}

\item{min}{\code{\link[=numeric]{numeric(1)}}\cr the minimum allowed value.}

\item{max}{\code{\link[=numeric]{numeric(1)}}\cr the maximum allowed value.}
}
\description{
Build a bit flag by checking for a range
}
\details{
when leaving either \code{min} or \code{max} at NULL they are set to
the minimum or maximum value of \code{test}, respectively, thereby carrying
out a "less than max" or "greater than min" operation.
}
