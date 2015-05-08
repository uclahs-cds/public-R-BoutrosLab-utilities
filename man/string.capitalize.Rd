\name{string.capitalize}
\alias{string.capitalize}
\title{Capitalize the words in a character string}
\description{This function takes a character string and capitalizes either the first word or all the words in it as desired.}
\usage{
string.capitalize(input.string, all.words);
}
\arguments{
    \item{input.string}{A character string to be capitalize}
    \item{all.words}{A boolean toggle indicating whether all the words should be capitalized (TRUE) or just the first one (FALSE)}
}
\details{}
\value{A capitalized character string}
\author{Michal Grzadkowski}
\examples{
# capitalizing the first/only word
string.capitalize('this is a sentence.', all.words = FALSE);
string.capitalize('attenborough');

# capitalizing all the words
string.capitalize('she sells seashells by the seashore', all.words = TRUE);
}
\keyword{utilities}

