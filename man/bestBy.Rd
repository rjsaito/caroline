\name{bestBy}
\alias{bestBy}
\author{David Schruth <dschruth@u.washington.edu>}
\title{Create a named vector from a dataframe, table or vector}
\description{
Finding the an extreme record for each group within a dataset is a more challenging routine task in R and SQL. 
This function provides a easy interface to that functionality either using R (fast for small data frames) or SQL (fastest for large data)
}
\usage{
bestBy(df, by, clmns=names(df), best, inverse=FALSE, sql=FALSE)
}
\arguments{
\item{df}{a data frame.}
\item{by}{the factor (or name of a factor in df) used to determine the grouping.}
\item{clmns}{the colums to include in the output.}
\item{best}{the column to sort on (both globally and for each sub/group)}
\item{inverse}{the sorting order of the sort column as specified by 'best'}
\item{sql}{whether or not to use SQLite to perform the operation.}}
\value{
A data frame of 'best' records from each factor level
}
\seealso{
\link{groupBy}
}
\examples{

blast.results <- data.frame(score=c(1,2,34,4,5,3,23), 
                            query=c('z','x','y','z','x','y','z'), 
                            target=c('a','b','c','d','e','f','g')
                            )
best.hits.R <- bestBy(blast.results, by='query', best='score', inverse=TRUE)
best.hits.R
## or using SQLite
best.hits.sql <- bestBy(blast.results, by='query', best='score', inverse=TRUE, sql=TRUE)
best.hits.sql

}
