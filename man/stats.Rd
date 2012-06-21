\name{stats}
\alias{stats}
\author{Dave McArthur <dmca@ucla.edu>}
\title{Descriptive Statistics}
\description{
adapted from http://legacy.ncsu.edu/ST370/distance/rlab/
}
\usage{
stats(x, by, quantiles=c(.25,.75))
}
\arguments{
\item{x}{list of distributions to characterize}
\item{by}{grouping variable presumes that x is univariate}
\item{quantiles}{any pair of values >0 : <1}
}
\value{
statistics on each distribution
}
\seealso{
violins, summary
}



