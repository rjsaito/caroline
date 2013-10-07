\name{sstable}
\alias{sstable}
\author{David Schruth}
\title{Sum Sorted Tabulation}
\description{
A wrapper for the "table()" function that also calculates the row-wise sum and sorts by the new column.  
}
\usage{
sstable(x, idx.clmns, ct.clmns = NULL, na.label = "NA")
}
\arguments{
\item{x}{list of vectors or a dataframe}
\item{idx.clmns}{index columns}
\item{ct.clmns}{count columns}
\item{na.label}{row label used for na columns}
}
\seealso{
ledghead, table, order, sort
}
\value{
A dataframe sorted by the count columns.
}
\examples{

  e <- data.frame(a=runif(12),b=runif(12), z=rep(letters[13:18],2),w=rep(letters[20:23],3))
  e <- data.frame(a=runif(10),b=runif(10), z=rep(letters[12:16],2),w=rep(letters[20:24],2))
  sstable(e, idx.clmns=c('z','w'), ct.clmns='a')
  sstable(e, idx.clmns=c('z'), ct.clmns=c('a','b'))
  sstable(e, idx.clmns=c('z','w'))
  e <- data.frame(a=10,b=0, z=as.factor(NA))
  sstable(e, 'z', c('a','b'))
  e <- data.frame(a=10,b=0, z=NA, w=NA)
  sstable(e, 'z', c('a','b'))
  e <- data.frame(a=runif(10),b=runif(10),m=rep(c('one','two'),5), 
                  z=factor(rep('z',10), levels=c('z','x')))
  sstable(e, idx.clmns=c('m','z'))
  
  
}
