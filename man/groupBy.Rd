\name{groupBy}
\alias{groupBy}
\title{Group a datafame by a factor and perform aggreate functions.}
\description{
The R equvalent of a SQL 'group by' call .}
\usage{
groupBy(df, by, clmns=names(df), aggregation=c('sum','mean','max','min','nrow','concat','none'), concat.sep=';', sql=FALSE)
}
\arguments{
\item{df}{a data frame.}
\item{by}{the factor (or name of a factor in df) used to determine the grouping.}
\item{clmns}{the colums to include in the output.}
\item{aggregation}{the functions to perform on the output (default is to sum).}
\item{concat.sep}{string delimiter for columns aggregated via concatenation.}
\item{sql}{whether or not to use SQLite to perform the grouping (not yet implimented).}
}
\value{
an summary/aggregate dataframe
}
\examples{
df <- data.frame(a=runif(12),b=runif(12), z=rep(letters[13:18],2),w=rep(letters[20:23],3))

groupBy(df=df, by='w', clmns=c('a','b','a','z','w'), aggregation=c('sum','mean','min','concat','nrow'))
# or using SQL
groupBy(df=df, by='w', clmns=c('a','b','a','z','w'), aggregation=c('sum','mean','min','concat','nrow'), sql=TRUE)

}
\keyword{}

