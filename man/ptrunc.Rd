% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-sfmisc.R
\name{ptrunc}
\alias{ptrunc}
\title{Paste and Truncate}
\usage{
ptrunc(..., width = 40L, sep = ", ", collapse = ", ", dots = " ...")
}
\arguments{
\item{...}{one or more \R objects, to be converted to character vectors.}

\item{width}{(maximum) width of result}

\item{sep}{a character string to separate the terms.  Not
    \code{\link[base]{NA_character_}}.}

\item{collapse}{an optional character string to separate the results.  Not
    \code{\link[base]{NA_character_}}.}

\item{dots}{\code{character} scalar. String to use for ellipses}

\item{x}{a vector}
}
\value{
a \code{character} scalar
}
\description{
Paste and Truncate
}
\examples{
  ptrunc(month.abb)
  ptrunc(month.abb, month.name)
}
