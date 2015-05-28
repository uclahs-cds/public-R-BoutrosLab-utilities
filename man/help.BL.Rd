\name{help.BL}
\alias{help.BL}
\title{Documentation and Code search}
\description{Search documentation and code of list of packages.}
\usage{help.BL(pattern, packages = NULL, search.code = FALSE)}
\arguments{
    \item{pattern}{The pattern to search for}
    \item{packages}{The packages to search through}
    \item{search.code}{Whether the code should be searched}
}
\value{documentation and code search as a two item list of matrices. }
\author{Daryl Waggott}
\examples{
    help.BL("affy");
}
\keyword{help}
\keyword{search}
