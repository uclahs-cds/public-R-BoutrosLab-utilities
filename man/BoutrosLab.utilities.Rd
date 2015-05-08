\name{BoutrosLab.utilities-package}
\Rdversion{1.1}
\alias{BoutrosLab.utilities-package}
\alias{BoutrosLab.utilities}
\docType{package}
\title{Boutros Lab Utility Functions}
\description{A set of useful utility functions for R coding}
\details{
\tabular{ll}{
Package: \tab BoutrosLab.utilities\cr
Type: \tab Package\cr
Version: \tab 1.9.5\cr
Date: \tab 2013-05-30\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
GarbageCollect(x = 9);
generate.filename(project.stem, file.core, extension, file.date = Sys.Date());
read.config.file(config.file = "config.txt", convert.path = FALSE, 
    old.path = if(.Platform$OS.type == "unix") "I:" else "/mnt/isilon/private", 
    new.path = if(.Platform$OS.type == "unix") "/mnt/isilon/private" else "I:");
read.tabdelimited(file);
save.session.profile(filename);
trim.leading.trailing.whitespace(x);
trim.leading.whitespace(x);
trim.trailing.whitespace(x);
}
\author{
Author: Paul C. Boutros <Paul.Boutros@utoronto.ca>
Maintainer: Paul C. Boutros <Paul.Boutros@utoronto.ca>
}
\references{}
\keyword{ package }
\seealso{}
\examples{}
