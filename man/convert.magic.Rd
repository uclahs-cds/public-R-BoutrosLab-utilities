\name{convert.magic}
\alias{convert.magic}
\title{Converts columns in a dataframe to specified type.}
\description{Takes a dataframe, converts specified columns to specified type. See examples for regular use.}
\usage{convert.magic(obj, type)}
\arguments{
    \item{obj}{A dataframe}
    \item{type}{Type to convert to, 'factor', 'numeric' or 'character'}
}
\value{The dataframe but coerced to specified type.}
\author{Nicholas Harding}

\examples{
df <- data.frame(x = 1:10,
	y = rep(1:2, 5),
	k = rnorm(10, 5,2),
	z = rep(c(2010, 2012, 2011, 2010, 1999), 2),
	j = c(rep(c("a", "b", "c"), 3), "d")
	);

str(df);
str(convert.magic(df, "character"));
str(convert.magic(df, "factor"));

# or use specific columns only!
df[, c("x", "y")] <- convert.magic(df[, c("x", "y")], "factor");
}
\keyword{ type }
\keyword{ factor }
\keyword{ convert }

