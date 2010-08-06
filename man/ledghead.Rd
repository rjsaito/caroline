\name{ledghead}
\alias{ledghead}
\author{David Schruth <dschruth@u.washington.edu>}
\title{Generate a color coded ledgend dataframe via head and sum.}
\description{
'ledghead' is part 'head' and part 'summary'.  It works best on a sorted dataframe where all you are interestesd in only the most (or least) abundant rows.  
An ideal place to use it is in a legend for ~lognormally distributed data.
Additionally, an optional row-wise color coding column is added (the color 'gray' is used for missing row names).
}
\usage{
ledghead(x, n=6, colors=TRUE, tabulate=FALSE)
}
\arguments{
\item{x}{dataframe or table you wish to summarize}
\item{n}{The number of rows you wish to display as is.}
\item{colors}{list of vectors or a dataframe}
\item{tabulate}{the column name to tabulate on if x is an untabulated dataframe and FALSE otherwise}
}
\value{
A truncated dataframe with a new bottom row summarizing all the truncated ones.
}
\seealso{
summary, head, sumSortedTable
}
\examples{
}
\keyword{}
