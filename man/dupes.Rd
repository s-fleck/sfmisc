% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-sfmisc.R
\name{dupes}
\alias{dupes}
\title{Return (unique) duplicated elements of a vector or rows of a data.frame}
\usage{
dupes(x, ...)
}
\arguments{
\item{x}{an \link{atomic} vector or \link{data.frame}}

\item{...}{passed on to \code{\link[=duplicated]{duplicated()}}}
}
\description{
For every element/row of \code{x} that has at least one duplicate, return one
instance of that element.
}
\examples{
dupes(c(1, 1, 1, 2))
dupes(cars[c(1, 1, 1, 2), ])
}
