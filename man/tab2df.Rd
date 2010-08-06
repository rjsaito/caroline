\name{tab2df}
\alias{tab2df}
\title{Table to Data Frame}
\description{
Convert a table to a dataframe while perserving the same number of columns and rows and names of each.
}
\usage{
tab2df(x, ...)
}
\arguments{
\item{x}{a table class object (output from the table command).}
\item{...}{other arguments passed to data.frame(...).}
}
\seealso{
table
}
\examples{
tab2df(table(c(1,2,1,1,1,2,2,2,2),c('a','b','b','a','b','b','a','b','b')))
}
\keyword{}

