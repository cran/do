% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Replace0.R
\name{Replace0}
\alias{Replace0}
\title{Replaced by Empty}
\usage{
Replace0(data, ...)
}
\arguments{
\item{data}{can be number, strings, verctors, dataframe or matrix.}

\item{...}{replaced stings}
}
\value{
replaced data
}
\description{
Replaced by Empty
}
\examples{
Replace0(data = 232,2)
Replace0(data = c(232,'a4b'),2,'.*4')

df = data.frame(
  a = c(232, 452),
  b = c("nba", "cba")
)
Replace0(data = df, 2,'a')

}
