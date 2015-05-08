\name{strsplit.to.matrix}
\alias{strsplit.to.matrix}
\title{Split string into a matrix}
\description{
Split string into a matrix with the several element selection strategies such as max, min and complex indices.
}
\usage{
strsplit.to.matrix(x, split, fixed = FALSE, perl = FALSE, select.cols = "max")
}
\arguments{
    \item{x}{A character vector}
    \item{split}{A character used for splitting}
    \item{fixed}{Should a fixed string be used}
    \item{perl}{Should perl regex be used}
    \item{select.cols}{What columns should be returned.  This is used when the result of parsing the vector of strings is a ragged array i.e. some strings return more splits then others.  The default is max, which makes a matrix with the number of rows equal to the longest split.  min is another option.  Alternatively, you can use a numeric vector that corresponds to both foward and reverse (prfixed with -) indices. i.e. c(1,-1) for the first and last value. }
}
\details{}
\value{}
\author{Daryl Waggott}
\examples{}
\keyword{strsplit}
