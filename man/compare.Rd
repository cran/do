% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare.R
\name{compare}
\alias{compare}
\alias{equal}
\alias{over}
\alias{lower}
\title{Compare two vectors}
\usage{
equal(a, b)

over(a, b)

lower(a, b)
}
\arguments{
\item{a}{one vector}

\item{b}{the other vector}
}
\value{
the compared object
}
\description{
Compare two vectors
}
\examples{
equal(letters,c('a','b'))
over(1:10,5)
over(1:10,5)
}
