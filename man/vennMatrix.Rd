\name{vennMatrix}
\alias{vennMatrix}
\author{David Schruth <dschruth@u.washington.edu>}
\title{Create a Venn Ready Matrix out of a List of Factors}
\description{
The limma package has great functions for making venn diagrams from a matrix.  This function is provides upstream functionality to turn a list of factors into this required input format.
}
\usage{
vennMatrix(l)
}
\arguments{
\item{l}{a named list of factors}
}
\value{
a matrix with columns for list elements and rows with globally unique factor levels
}
\seealso{
venCounts
}
\examples{

l <- list(a=factor('x','y','z'), b=factor('w','x','v'))

vennMatrix(l)

}
