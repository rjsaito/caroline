\name{usr2lims}
\alias{usr2lims}
\title{Grab and adjust the current plot dimensions}
\description{
This is a simple function which grabs the current plot dimentions and adjusts them by shrinking them by 4% in each direction.
}
\usage{
usr2lims(adj=.04)
}
\arguments{
\item{adj}{The automatic adjustment factor 'plot' adds to buffer the specified plot dimentions.}
}
\value{
A 2 item (x and y) list of 2 item (min and max) vectors for x and y limits of the current plot area
}
\seealso{
par
}
\examples{
plot(c(0,1), c(0,1))
usr2lims()
}
