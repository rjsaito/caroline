\name{regroupDF}
\alias{regroupDF}
\title{Regroup a dataframe by a new trumping factor (NA's allowed).}
\description{
Used to group a dataframe of numbers by a factor that need not be the same length.}
\usage{
regroupDF(df, old, new, cols, funcs=rep('sum',length(cols)))
}
\arguments{
\item{df}{a dataframe.}
\item{old}{the ids to match the rows in df to the 'new' grouping ids.}
\item{new}{the new ids (must be a vector of the same length as 'old'.}
\item{cols}{the colums to include in the output.}
\item{funcs}{the functions to perform on the output (default is to sum) .}
}
\seealso{
}
\examples{
}
\keyword{}

