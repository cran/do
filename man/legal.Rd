% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/legal.R
\name{legal}
\alias{legal}
\title{Check legal character
Whether the character is legal for names in dataframe or formula}
\usage{
legal(...)
}
\arguments{
\item{...}{one or more string}
}
\value{
logical, TRUE means legal.
}
\description{
Check legal character
Whether the character is legal for names in dataframe or formula
}
\examples{
legal('a','b','a b')
}
