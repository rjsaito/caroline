\name{nerge}
\alias{nerge}
\title{Named Merge}
\description{
This function is a wrapper for merge that supports merging multiple vectors and or dataframes
}
\usage{
nerge(l, ...)
}
\arguments{
\item{l}{A named list of named vectors (and/or dataframes)}
\item{...}{Other parameters passed on to each sub-merge}
}
\seealso{
\code{\link{merge}}.
}
\examples{

df <- data.frame(a=c(6,7,8), b=c(9,8,7))
rownames(df) <- c('a','d','c')

l <- list(x=nv(c(1,2),c('a','b')),y=nv(c(2,3),c('b','d')),z=nv(c(4,1),c('c','d') ), w=df)
nerge(l)


l2 <- list(a=nv(c(1.23, 1.423, 2.343), c('z','y','x')),b=nv(c(6.34,7.34, 12.545),c('z','w','y')))
nerge(l2, all=TRUE)


}
\keyword{manip}

