\name{trim.leading.whitespace}
\alias{trim.leading.whitespace}
\title{Strip leading whitespace}
\description{The function strips leading whitespace from strings}
\usage{
trim.leading.whitespace(x);
}
\arguments{
  \item{x}{String to be stripped}
}
\details{This is a very simple wrapper function, and should *not* be extended!}
\value{Returns the string after leading-whitespace has been removed, or NA if passed a non-character}
\author{Paul Boutros}
\examples{
trim.leading.whitespace('    hi!');
}
\keyword{utilities}
