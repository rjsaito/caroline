\name{write.delim}
\alias{write.delim}
\title{Write a (tab) delimited text file.}
\description{
A simple wrapper for write.table with the same options as read.delim
}
\usage{
write.delim(df, file, quote = FALSE, row.names = FALSE, sep = "\t", ...)
}
\arguments{
\item{df}{a dataframe.}
\item{file}{outputfile path.}
\item{quote}{should elements of the dataframe be quoted for output.}
\item{row.names}{should the output include rownames.}
\item{sep}{the delimiter between fields.}
\item{...}{other parameters passed to write.table.}
}
\value{
A tab delimited text file
}
\examples{
## Not run: 
x <- data.frame(a = I("a \" quote"), b = pi)
write.delim(x, file = "foo.tab")
## End(Not run)
}
\keyword{}

