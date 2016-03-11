
\name{confusion.matrix}
\alias{confusion.matrix}
\title{Confusion Matrix}
\description{This function generates a confusion matrix from a vector of observed values
 and a vector of predictions. The returned data frame has attributes for common metrics
 used when evaluating binary classifiers.}
\usage{
confusion.matrix(response = response, truth = truth, positive.value = 1);
}
\arguments{
    \item{prediction}{Vector of predicted values}
    \item{truth}{Vector of actual observations}
    \item{positive.value}{(Optional) Value that will be considered to be the positive class}
}
\value{Dataframe containing confusion matrix, with attributes}
\author{Christopher Lalansingh}
\examples{
# define truth and response
truth <- c(0,1,0,0,1,1);
response <- c(0,0,1,0,1,1);

# get confusion matrix
cm <- confusion.matrix(response = response, truth = truth, positive.value = 1);

# view confusion matrix
print(cm);

# view attributes
str(cm);
}
\keyword{utilities}
