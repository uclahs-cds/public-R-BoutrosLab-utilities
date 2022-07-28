\name{trim.trailing.whitespace}
\alias{trim.trailing.whitespace}
\title{Strip trailing whitespace}
\description{The function strips trailing whitespace from strings}
\usage{
trim.trailing.whitespace(x);
}
\arguments{
    \item{x}{String to be stripped}
}
\details{This is a very simple wrapper function, and should *not* be extended!}
\value{Returns the string after trailing-whitespace has been removed, or NA if passed a non-character}
\author{Paul Boutros}
\examples{
trim.trailing.whitespace('hi!   ');
}
\keyword{utilities}
