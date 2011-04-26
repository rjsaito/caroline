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
leghead(x, n=7, tabulate=FALSE, colors=TRUE, na.name='NA', na.col = "white", other.col = "gray",  na.last = TRUE)
}
\arguments{
\item{x}{dataframe or table you wish to summarize}
\item{n}{the number of rows you wish to display as is}
\item{colors}{list of vectors or a dataframe}
\item{tabulate}{the column name to tabulate on if x is an untabulated dataframe and FALSE otherwise}
\item{na.name}{the new rowname for a row with a missing name}
\item{na.col}{color for rows labeled as 'NA'}
\item{other.col}{color for the rows labeled as 'unknown'}
\item{na.last}{boolean specifying if the na category should be listed last in the table.}
}
\value{
A truncated dataframe with a new bottom row summarizing all the truncated ones.
}
\seealso{
summary, head, sumSortedTable
}
\examples{
 e <- data.frame(a=runif(12),b=runif(12), z=rep(letters[13:18],2),w=rep(letters[20:23],3))
tab <- sstable(e, idx.clmns=c('z'), ct.clmns=c('a','b'))
lh <- leghead(tab)
plot(x=lh$a, y=lh$b, cex=lh$sum*3, col=lh$color, pch=20)
legend('topleft',legend=rownames(lh), col=lh$color, pch=20)
}
\keyword{}
