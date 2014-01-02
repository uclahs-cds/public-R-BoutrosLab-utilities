\name{find.package.file}
\alias{find.package.file}
\title{Find file installed in package}
\description{The function find the location of a file that was installed with a package (i.e. files located inside of the package's inst/ directory)}
\usage{
find.package.file(package, file);
}
\arguments{
  \item{package}{A string containing the package name to check}
  \item{file}{A string containing the name of the file}
}
\details{This will only return one file path under most circumstances, unless you have multiple copies of the package in your .libPaths()}
\value{Returns a vector of paths to files installed with the package.}
\author{Francis Nguyen}
\examples{
filename <- find.package.file('BoutrosLab.utilities', 'NAMESPACE');
filename <- find.package.file('base', 'DESCRPTION');
}
\keyword{utilities}
