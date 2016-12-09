\name{evaluate.and.save}
\alias{evaluate.and.save}
\title{Save and read R object from disk}
\description{This function generates an R object if it does not exist, but reads the object from disk if it exists.}
\usage{
evaluate.and.save(object.path, object.constructor, should.print = TRUE, should.flush = FALSE);
}
\arguments{
    \item{object.path}{The path for the R object}
    \item{object.constructor}{A function that returns the R object}
    \item{should.print}{A logical indicating whether to print disk activity}
    \item{should.flush}{A logical indicating whether to flush the object stored on disk}
}
\value{R Object}
\author{Nathaniel Schmitz}
\examples{
# save a large csv file to a dataframe for fast subsequent loading
evaluate.and.save(
	'large_table.rds',
	function() {
		data.frame(
			row = 1:1000,
			value = rnorm(4)
			);
		},
	should.print = TRUE,
	should.flush = FALSE
	);
}
\keyword{utilities}

