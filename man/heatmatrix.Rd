\name{heatmatrix}
\alias{heatmatrix}
\title{Simple Heatmap Plot}
\description{
This is a very simplified heatmap function: basically a convienent wrapper around the 'image' function.  
}
\usage{
heatmatrix(x, values=TRUE, clp=c('bottom','top'), rlp=c('left','right'), 
                             xadj=.02, yadj=.3, ylab.cntr=FALSE, cex=1, cex.axis=1, ...)
}
\arguments{
\item{x}{A matrix}
\item{values}{boolean: should the values be plotted over each cell?}
\item{clp}{column label position: either 'bottom' or 'top'.}
\item{rlp}{row label position: either 'right' or 'left'}
\item{xadj}{x-adjust of the row labels}
\item{yadj}{y-adjust of the column labels.}
\item{ylab.cntr}{boolean for justification of row labels.}
\item{cex,}{character expansion factor for values in cells if values == true}
\item{cex.axis}{character expansion factor for axis tick mark labels}
\item{...}{other paramters passed on to image()}
}
\seealso{
\link{image}, \link[stats]{heatmap}, \link[gplots]{heatmap.2}
}
\value{
a heatmap plot
}
\examples{

data(mtcars)
x  <- as.matrix(mtcars)

heatmatrix(x)

}

