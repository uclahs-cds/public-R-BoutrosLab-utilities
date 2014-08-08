\name{trim.leading.trailing.whitespace}
\alias{trim.leading.trailing.whitespace}
\title{Strip leading & trailing whitespace}
\description{The function strips leading & trailing whitespace from strings}
\usage{
trim.leading.trailing.whitespace(x);
}
\arguments{
  \item{x}{String to be stripped}
}
\details{This is a very simple wrapper function, and should *not* be extended!}
\value{Returns the string after leading & trailing whitespace has been removed, or NA if passed a non-character}
\author{Paul Boutros}
\examples{
trim.leading.trailing.whitespace('  hi!   ');
}
\keyword{utilities}
