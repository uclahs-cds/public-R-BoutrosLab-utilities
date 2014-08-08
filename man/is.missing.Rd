\name{is.missing}
\alias{is.missing}
\title{
Check proportion of missing
}
\description{
Get the percent missing or return a logical if missing is below a specified threshold.
}
\usage{
is.missing(x, rows = TRUE, missing.value = NA, threshold = NULL);
}
\arguments{
  \item{x}{A vector or matrix}
  \item{rows}{Should the summary be over rows or columns}
  \item{missing.value}{What value is considered missing.  Can be a vector of values that include NA.}
  \item{threshold}{A value between 0 and 1 which determines the missing threshold. }
}
\details{

}
\value{
Defaults to returning a percent of row wise missing.  If a threshold is given between 0-1 then the output is logical.
}
\author{
Daryl Waggott
}
\examples{
}
\keyword{missing}
