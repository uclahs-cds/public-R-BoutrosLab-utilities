\name{get.most.recent.dir}
\alias{get.most.recent.dir}
\title{Returns most recent valid file or directory given a char string}
\description{This function tests filenames in reverse date order, given an input containing the string 'DATE'.}
\usage{
get.most.recent.dir(dir.path, max.days=100);
}
\arguments{
  \item{dir.path}{Name of the dir/file. Should contain the string 'DATE'}
  \item{max.days}{Maximum days back to test.}
}
\details{}
\value{A valid filepath}
\author{Nicholas Harding}
\examples{
# create a file with date 10 days old and find that file.
name <- paste(as.Date(as.double(Sys.Date()) - 10, origin="1970-01-01"),'_FILE',sep='')
print(name);
file.create(name);
file.name <- get.most.recent.dir('DATE_FILE');
print(file.name);
file.remove(name);
}
\keyword{utilities}
