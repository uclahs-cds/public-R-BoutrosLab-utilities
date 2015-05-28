\name{read.config.file}
\alias{read.config.file}
\title{Read Configuration File}
\description{The function reads a file with a list of configuration options.  File paths for data input and output are common items.}
\usage{
read.config.file(config.file, convert.path, old.path, new.path);
}
\arguments{
    \item{config.file}{Name of the file containing the configuration data to be read}
    \item{convert.path}{Should file paths be converted for platform compatability.  A simple search and replace of the old.string with the new.string is completed on the config file.  The default is FALSE.}
    \item{old.path}{Pattern.  The default is "I:" if you are on a unix machine and "/mnt/isilon/private" if you are on windows.}
    \item{new.path}{Replacement. The default is "/mnt/isilon/private" if you are on a unix machine and "I:" if you are on windows.}	
}
\value{Returns a list of the parameters}
\author{Mehrdad Shamsi}
\keyword{utilities}
