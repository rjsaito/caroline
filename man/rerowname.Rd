\name{rerowname}
\alias{rerowname}
\title{Rename select rows of a dataframe}
\description{
Used to easily rename the rows of a dataframe.
}
\usage{
rerowname(df, old='NA', new='unknown')
}
\arguments{
\item{df}{A dataframe with rownames.}
\item{old}{The row name to be replaced.}
\item{new}{The replacment row name.}
}
\value{
A dataframe with one new rowname
}
\examples{

df <- data.frame(a=c(1,2,3), b=c('x','y','z'), c=c(5,3,2))
rownames(df) <- c('p','q','NA')
rerowname(df)

}
\keyword{manip}

