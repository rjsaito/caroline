\name{groupBy}
\alias{groupBy}
\title{Group a datafame by a factor and perform aggreate functions.}
\description{
The R equvalent of a SQL 'group by' call.  }
\usage{
groupBy(df, by, aggregation,  clmns=names(df), collapse=',', distinct=FALSE, sql=FALSE, full.names=FALSE, ...)
}
\arguments{
\item{df}{a data frame.}
\item{by}{the factor (or name of a factor in df) used to determine the grouping.}
\item{aggregation}{the functions to perform on the output (default is to sum). Suggested functions are: 'sum','mean','var','sd','max','min','length','paste',NULL.}
\item{clmns}{the colums to include in the output.}
\item{collapse}{string delimiter for columns aggregated via 'paste' concatenation.}
\item{distinct}{used in conjunction with paste and collapse to only return unique elements in a delimited concatenated string}
\item{sql}{whether or not to use SQLite to perform the grouping (not yet implimented).}
\item{full.names}{names of the aggregation functions should be appended to the output column names}
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

groupBy(df=df, by='w', clmns=c(rep(c('a','b'),3),'z','w'), aggregation=c('sum','mean','var','sd','min','max','paste','length'), full.names=TRUE, na.rm=TRUE)
# or using SQLite
groupBy(df=df, by='w', clmns=c(rep(c('a','b'),2),'z','w'), aggregation=c('sum','mean','min','max','paste','length'), full.names=TRUE, sql=TRUE)


## passing a custom function
meantop <- function(x,n=2, ...)
  mean(x[order(x, decreasing=TRUE)][1:n], ...)
  
groupBy(df, by='w', aggregation=rep(c('mean','max','meantop'),2), clmns=rep(c('a','b'),3), na.rm=TRUE)

}