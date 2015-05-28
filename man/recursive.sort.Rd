\name{recursive.sort}
\alias{recursive.sort}
\title{Sort a data.frame by all rows or columns}
\description{This function sort a data.frame by all rows or columns}
\usage{
recursive.sort(x, by="col",na.last=TRUE, decreasing=FALSE, skip=NULL, reverse = FALSE, group = NULL, alternate.dim.order.func = NULL, alternate.dim.order.func.rev = FALSE)
}

\arguments{
    \item{x}{data.frame to sort}
    \item{by}{Sort by (col)umns  or row(s).  Defaults to columns.}
    \item{na.last}{see order}
    \item{decreasing}{reverse sort order}
    \item{skip}{row or column names to skip when creating the sort order}
    \item{reverse}{Should the sort order priority start from last row or column}
    \item{group}{a group vector or matrix which allows for nested ordering}
    \item{alternate.dim.order.func}{a function to be used for sort i.e. mean, sum}
    \item{alternate.dim.order.func.rev}{sort the alternate dimension in descending order using the results of the specified function}
}
\value{sorted data.frame}
\author{Daryl Waggott}
\examples{
dd <- data.frame(b = factor(c("Hi", "Med", "Hi", "Low"), 
      levels = c("Low", "Med", "Hi"), ordered = TRUE),
      x = c("A", "D", "A", "C"), y = c(8, 3, 9, 9),
      z = c(9, 1, 1, 2));

recursive.sort(dd,skip = "b");
recursive.sort(dd,by = "row");
}

\keyword{utilities}
