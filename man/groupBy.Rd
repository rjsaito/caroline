\name{groupBy}
\alias{groupBy}
\title{Group a datafame by a factor and perform aggreate functions.}
\description{
The R equvalent of a SQL 'group by' call .}
\usage{
groupBy(df, fact, cols, funcs=rep('sum',length(cols)))
}
\arguments{
\item{df}{a data frame.}
\item{fact}{the factor used to determine the grouping.}
	\item{cols}{the colums to include in the output.}
\item{funcs}{the functions to perform on the output (default is to sum) .}
}
\seealso{
}
\examples{
}
\keyword{}

