\name{groupBy}
\alias{groupBy}
\title{Group a datafame by a factor and perform aggreate functions.}
\description{
The R equvalent of a SQL 'group by' call .}
\usage{
groupBy(df, fact, clmns = names(df), funcs=rep('sum',length(clmns)))
}
\arguments{
\item{df}{a data frame.}
\item{fact}{the factor used to determine the grouping.}
\item{clmns}{the colums to include in the output.}
\item{funcs}{the functions to perform on the output (default is to sum) .}
}
\value{
an summary/aggregate dataframe
}
\examples{
 e <- data.frame(a=runif(12),b=runif(12), z=rep(letters[13:18],2),w=rep(letters[20:23],3))

groupBy(df=e, fact=e$w, clmns=c('a','b'), funcs=c('sum','prod'))

}
\keyword{}

