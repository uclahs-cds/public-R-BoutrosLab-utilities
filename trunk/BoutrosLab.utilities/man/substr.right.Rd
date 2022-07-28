\name{substr.right}
\alias{substr.right}
\title{Substring from the end of a string}
\description{This function takes a character string and a number of characters and substrings the character string from the end.}
\usage{
substr.right(input.string, length);
}
\arguments{
    \item{input.string}{A character string to substring}
    \item{length}{The length of the substring}
}
\value{A substring from input.string}
\author{Nathaniel Schmitz}
\examples{
# right substring with length 3 of 'Boutros Lab' is 'Lab'
substr.right('Boutros Lab', 3);
}
\keyword{utilities}

