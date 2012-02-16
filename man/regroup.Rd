\name{regroup}
\alias{regroup}
\title{Regroup a dataframe.}
\description{
Used to group a dataframe of numbers by a factor that need not be the same length.  Find the a factor in the old df and use it to group by the new trumping factor (NA's allowed)}
\usage{
regroup(df, old, new, clmns, funcs=rep('sum',length(clmns)), combine=TRUE)
}
\arguments{
\item{df}{a dataframe.}
\item{old}{the ids to match the rows in df to the 'new' grouping ids.}
\item{new}{the new ids (must be a vector of the same length as 'old'.}
\item{clmns}{the colums to include in the output.}
\item{funcs}{the functions to perform on the output (default is to sum) .}
\item{combine}{Determines wether to combine with existing groupings or to start fresh.}
}
\value{
a dataframe with number of rows equal to the number of factor levels in 'new'
}
\examples{

df <- data.frame(a=rnorm(20),b=rpois(20,1))

mapping <- data.frame(old=rownames(df), new=rep(c('a','b'),10))
regroup(df, old=mapping$old, new=mapping$new)
}

