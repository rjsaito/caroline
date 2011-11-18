\name{groupBy}
\alias{groupBy}
\title{Group a datafame by a factor and perform aggreate functions.}
\description{
The R equvalent of a SQL 'group by' call .}
\usage{
groupBy(df, by, clmns=names(df), aggregation=c('sum','mean','var','sd','max','min','count','concat','none'), concat.sep=';', sql=FALSE, full.names=FALSE, ...)
}
\arguments{
\item{df}{a data frame.}
\item{by}{the factor (or name of a factor in df) used to determine the grouping.}
\item{clmns}{the colums to include in the output.}
\item{aggregation}{the functions to perform on the output (default is to sum).}
\item{concat.sep}{string delimiter for columns aggregated via concatenation.}
\item{sql}{whether or not to use SQLite to perform the grouping (not yet implimented).}
\item{full.names}{should the names of the aggregation functions be appended to the output column names?}
\item{...}{additional parameters (such as na.rm) passed to the underlying aggregate functions.}
}
\value{
an summary/aggregate dataframe
}
\seealso{
\link[stats]{aggregate}, \link{bestBy}
}
\examples{
df <- data.frame(a=runif(12),b=c(runif(11),NA), z=rep(letters[13:18],2),w=rep(letters[20:23],3))

groupBy(df=df, by='w', clmns=c(rep(c('a','b'),3),'z','w'), aggregation=c('sum','mean','var','sd','min','max','concat','count'), full.names=TRUE, na.rm=TRUE)
# or using SQLite
groupBy(df=df, by='w', clmns=c(rep(c('a','b'),2),'z','w'), aggregation=c('sum','mean','min','max','concat','count'), full.names=TRUE, sql=TRUE)

}
\keyword{}

