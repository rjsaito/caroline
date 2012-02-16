\name{addFactLevs} 
\alias{addFactLevs}
\title{Add new levels to the Factors in a DataFrame.}
\description{
This function loops through all of the factor columns in a dataframe and adds new.levs to the factor levels list.
}
\usage{
addFactLevs(x, new.levs=NA)
}
\arguments{
\item{x}{a dataframe.}
\item{new.levs}{new levels to be added.}
}
\seealso{
\link[base]{factor}, \link[base]{levels}
}

\keyword{manip}

