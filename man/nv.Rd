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
\item{x}{The source dataframe, table, or vector}
\item{name}{The column name you would like to pull out as a named vector. OR the names of the vector (if x is a vector)}
}
\seealso{
\$
}
\examples{
}
\keyword{}
