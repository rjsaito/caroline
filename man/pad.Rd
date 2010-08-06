\name{pad}
\alias{pad}
\title{Pad a vector of numerical string with zeros.}
\author{Jeremy Tantrum}
\description{
This function helps to pad numbers on the left side with zeros so that 
they may be used to create strings used in filesystem names (for example). 
}
\usage{
pad(vect,np)
}
\arguments{
\item{vect}{a vector of strings representing numbers.}
\item{np}{number of zeros to pad to the left of the string.}
}
\value{
a (vector of) string(s) with np number of zeros padded on as a prefix
}
\examples{
pad(c(1,10,1000,10000), 4)
}
\keyword{manip}

