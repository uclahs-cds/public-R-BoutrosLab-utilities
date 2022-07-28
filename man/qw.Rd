\name{qw}
\alias{qw}
\title{Quote Word}
\description{This function mimics the perl 'qw' function. It splits a string into a vector of strings using space as a delimiter.}
\usage{
qw(x, split = NULL);
}
\arguments{
    \item{x}{String to split}
    \item{split}{Delimeter, tries to guess}
}
\value{Vector of strings}
\author{Nicholas J. Harding}
\examples{
# define string
my.string <- 'green red blue yellow orange';

# use a string directly
qw('green red blue yellow orange');

# use on an existing object
qw(my.string);
}
\keyword{utilities}
