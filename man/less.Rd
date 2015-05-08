\name{less}
\alias{less}
\title{Page through a data object}
\description{This function allows a user to page through a data object similar to the UNIX less command.}
\usage{
less(x);
}
\arguments{
    \item{x}{Name of the data object}
}
\details{}
\value{No return value}
\author{Richard de Borja}
\examples{

x <- rnorm(10000);
less(x);

}
\keyword{utilities}
