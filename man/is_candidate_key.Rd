% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-sfmisc.R
\name{is_candidate_key}
\alias{is_candidate_key}
\title{Test if a Vector or Combination of Vectors is a Candidate Key}
\usage{
is_candidate_key(x)
}
\arguments{
\item{x}{a atomic vector or a list of atomic vectors}
}
\value{
\code{TRUE/FALSE}
}
\description{
Checks if all elements of the atomic vector \code{x}, or the combination of
all elements of \code{x} if \code{x} is a \code{list}, are unique and neither \code{NA} or
\code{infinite}.
}
\examples{

is_candidate_key(c(1, 2, 3))
is_candidate_key(c(1, 2, NA))
is_candidate_key(c(1, 2, Inf))

td <- data.frame(
  x = 1:10,
  y = 1:2,
  z = 1:5
)

is_candidate_key(list(td$x, td$z))
# a data.frame is just a special list
is_candidate_key(td[, c("y", "z")])
}
