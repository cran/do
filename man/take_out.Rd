% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/take_out.R
\name{take_out}
\alias{take_out}
\title{Extract Some String}
\usage{
take_out(x, ..., type = "c")
}
\arguments{
\item{x}{string}

\item{...}{patterns of c('begin','after')}

\item{type}{any left characters of character or list}
}
\value{
characters
}
\description{
Extract Some String
}
\examples{
x='abdghtyu'
take_out(x,c('a','d'),c('h','u'))
}
