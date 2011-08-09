\name{tab2df}
\alias{tab2df}
\title{Table to Data Frame}
\description{
Convert a table to a dataframe while perserving the same number of columns and rows and names of each.
}
\usage{
tab2df(x, ...)
}
\arguments{
\item{x}{a table or matrix class object (output from the table command).}
\item{...}{other arguments passed to data.frame(...).}
}
\seealso{
table
}
\value{
a dataframe
}
\examples{

  x <- data.frame(a=runif(10),b=runif(10), z=rep(letters[1:5],2))
  as.data.frame(x)
  tab2df(x)
  x <- nv(rnorm(10), letters[1:10])
    as.data.frame(x)
  tab2df(x)
  x <- nv(rnorm(2), c('x.b','y.b'))
    as.data.frame(x)
  tab2df(x)
  x <- nv(rnorm(2), c('b.x','b.y'))
    as.data.frame(x)
  tab2df(x)
  e <- data.frame(a=runif(10),b=runif(10), z=rep(letters[13:17],2))
  x <- as.table(sapply(c('a','b'),function(cc) by(e[,'a'],list(e$z), sum)))
    as.data.frame(x)
  tab2df(x)
  x <- as.table(by(1:10, list(a=rep(1:5,2),b=rep(1:2,5)), sum))
    as.data.frame(x)
  tab2df(x)
  x <- as.table(nv(c(54,34), c('a','b')))
    as.data.frame(x)
  tab2df(x)

  x <- table(a='x',b='y')
  tab2df(x)
  
  }

