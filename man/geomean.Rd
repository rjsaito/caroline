\name{geomean}
\alias{geomean}
\author{ David Schruth <dschruth@u.washington.edu>}
\title{Calculate the Geometric Mean}
\description{
A trivial one-line function for exp(mean(log()))
}
\usage{
geomean(x)
}
\arguments{
\item{x}{a vector of numeric values}
}
\seealso{
geometric.mean
}
\value{
the geometric mean (a scalar value)
}
\examples{

geomean(rnorm(20,5))

}
