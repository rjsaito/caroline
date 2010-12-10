\name{pct}
\alias{pct}
\title{Add Percentage Columns to a Dataframe}
\description{
This function will add extra columns to an existing dataframe.  The second argument 'clmn' should specify which column(s) of the dataframe the percentage should be calculated by dividing each column's row-element by it's sum.}
\usage{
pct(df, clmns)
}
\arguments{
\item{df}{A dataframe with numeric columns.}
\item{clmns}{the names of the columns for which the percentage column should be calculated from.}
}
\value{
The original dataframe plus extra percentage columns corresponding to original columns in the dataframe.
}
\examples{

df <- data.frame(a=c(1,2,3), b=c('x','y','z'), c=c(5,3,2))
pct(df, c('a','c'))

}
\keyword{manip}

