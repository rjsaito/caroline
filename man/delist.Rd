\name{delist}
\alias{delist}
\author{David Schruth <dschruth@u.washington.edu>}
\title{Convert a named list into new objects in memory.}
\description{
This function uses the 'assign' function to turn each object element of the list into a new object in the global environment.
}
\usage{
delist(L)
}
\arguments{
\item{L}{list of named objects}
}
\value{
new variables in the global environment
}
\seealso{
\link{assign}
}
\examples{
L <- list(a=c(12,123,1), b='alfalfa', c=c('a','b','c'))
delist(L)
print(a)
print(b)
print(c)
}
