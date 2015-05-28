\name{sample.subsets}
\alias{sample.subsets}
\title{Sample unique subsets}
\description{This function picks random unique subsets of a vector.}
\usage{
sample.subsets(x, size, n);
}
\arguments{
    \item{x}{The vector to sample subsets from}
    \item{size}{The size of each subset}
    \item{n}{The number of subsets to sample}
}
\value{A list of length n consisting of elements of length size}
\author{Michal Grzadkowski}
\examples{
# sample 5 unique pairs from the first three integers
sample.subsets(1:10, 2, 5);

# sample 12 unique subsets of size four from the uppercase alphabet
sample.subsets(LETTERS, 4, 12);
}
\keyword{utilities}
