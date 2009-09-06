\name{volins}
\alias{violins}
\title{Plot a Series of Vioplot Violins}
\description{
This is an improvment on the original vioplot function (combination of a box plot and a kernel density plot).  You can now pass a list of violins and a list of colors.
}
\usage{
violins(x, range = 1.5, h = NULL, ylim = NULL, names = NULL, 
    horizontal = FALSE, col = "magenta", border = "black", lty = 1, 
    lwd = 1, rectCol = "black", colMed = "white", pchMed = 19, 
    at, add = FALSE, wex = 1, drawRect = TRUE, main = "vioplot") 
}
\arguments{
\item{x}{list of vectors.}
\item{range}{a factor to calculate the upper/lower adjacent values.}
\item{h}{the height for the density estimator, if omit as explained in sm.density, h will be set to an optimum.}
\item{ylim}{y limits.}
\item{names}{one label, or a vector of labels for the datas must match the number of datas given.}
\item{horizontal}{description of var 1.}
\item{col,border,lty,lwd}{Graphical parameters for the violin passed to lines and polygon.}
\item{rectCol,colMed,pchMed}{Graphical parameters to control the look of the box.}
\item{at}{position of each violin. Default to 1:n}
\item{add}{logical. if FALSE (default) a new plot is created}
\item{wex}{relative expansion of the violin. }
\item{drawRect}{logical. the box is drawn if TRUE.}
\item{main}{main title for the plot.}
}
\seealso{
vioplot
}
\examples{
}
\keyword{}
