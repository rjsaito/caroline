\name{leghead}
\alias{leghead}
\author{David Schruth <dschruth@u.washington.edu>}
\title{Generate a Color Coded Legend dataframe via head and sum.}
\description{
'leghead' is part 'head' and part 'summary'.  It works best on a sorted dataframe where all you are interestesd in only the most (or least) abundant rows.  
An ideal place to use it is in a legend for ~lognormally distributed data.
Additionally, an optional row-wise color coding column is added (the color 'gray' is used for missing row names).
}
\usage{
leghead(x, n=7, tabulate=FALSE, colors=TRUE, na.col = "gray", other.col = "white")
}
\arguments{
\item{x}{dataframe or table you wish to summarize}
\item{n}{The number of rows you wish to display as is.}
\item{colors}{list of vectors or a dataframe}
\item{tabulate}{the column name to tabulate on if x is an untabulated dataframe and FALSE otherwise}
\item{na.col}{color for rows labeled as 'NA'}
\item{other.col}{color for the rows labeled as 'unknown'}
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
