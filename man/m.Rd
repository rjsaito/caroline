\name{m}
\alias{m}
\title{Regexp Match Operator}
\description{
A grep  sub like function that actually returns the n back-referenced *matches* 
(as columns in a datframe of the same length as the input vector). This function
is more geared towards *extracting* than cleaning ... 
even though it uses sub (a cleaning function) to do so.
}
\usage{
m(pattern, vect, names="V", types="character")
}
\arguments{
\item{pattern}{A regular expression pattern with at least one back reference.}
\item{vect}{A string or vector of strings one which to apply the pattern match.}
\item{names}{The vector of names of the new variables to be created out of vect. Must be the same length as vect.}
\item{types}{The vector of types of the new variables to be created out of vect. Must be the same lenght as vect.}
}
\seealso{
\code{\link{gsub},\link{regexpr}, \link{grep}, \link{gregexpr}}.
}
\examples{
}
\keyword{manip}

