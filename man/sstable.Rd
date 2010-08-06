\name{sstable}
\alias{sstable}
\author{David Schruth <dschruth@u.washington.edu>}
\title{Sum Sorted Tabulation}
\description{
A wrapper for the table command that also calculates the row-wise sum and sorts by the new column.  
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
\examples{
}
\keyword{}
