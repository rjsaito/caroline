\name{nv}
\alias{nv}
\author{David Schruth <dschruth@u.washington.edu>}
\title{Create a named vector from a dataframe, table or vector}
\description{
The '$' or "[," operators for dataframes and tables do not carry along with them the row names.  This function provides a solution for this problem.  Optionally, you can pass a vector to x and use name to name it (great for default parameters)
}
\usage{
nv(x, name)
}
\arguments{
\item{x}{The source dataframe, table, vector, or factor}
\item{name}{The column name you would like to pull out as a named vector. OR the names of the vector (if x is a vector)}
}
\value{
a named vector or factor
}
\seealso{
\link{vector},\link{name}
}
\examples{

## example 1: pulling a row.named vector out of a dataframe
df <- data.frame(a=c(1,2,34,4,5,3,23), b=c('z','x','y','z','x','n','p'))
rownames(df) <- letters[1:nrow(df)]
nv(df,'a')
nv(df,'b')

## example 2: a naming vectors from scratch
nv(c(1,2,3), c('a','b','c'))
nv(df$a, df$b)

}
