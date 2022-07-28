
\name{calculate.mcc}
\alias{calculate.mcc}
\title{Calculate Matthew's Correlation Coefficient}
\description{This function returns the Matthew's Correlation Coefficient (MCC) of the model 
given a vector predicted classes and a vector of true classes. MCC is a useful metric in 
assessing model performance when classes are imbalanced.}
\usage{
calculate.mcc(predicted.class, true.class);
}
\arguments{
    \item{predicted.class}{Vector of predicted classes}
    \item{true.class}{Vector of true classes}
}
\value{MCC, numeric value}
\author{Ren Sun}
\examples{
# define predictions and truths
truth <- c(rep(0, 50), rep(1, 10));
prediction <- c(rep(0, 40), rep(1, 5), rep(0, 5), rep(1, 6), rep(0, 4));

# calculate mcc
mcc <- calculate.mcc(prediction, truth);
print(mcc);

}
\keyword{utilities}
