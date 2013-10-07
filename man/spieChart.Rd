\name{spie}

\alias{spie}
\alias{plot.spie}

\title{Spie charts}
\description{Spie Chart}

\usage{ 
 spie(p1, p2, init.angle=pi, multi, col = rainbow(length(x$radii)), bg=col, lwd=2, 
              pie.labs=TRUE, grid=TRUE, grid.labs=TRUE, scale=TRUE, p1.circle=TRUE)
}

\arguments{

  \item{p1}{a positive numeric vector.}
  \item{p2}{a positive numeric vector. Angles are the same than those used for the first pie but radii change according to the values in .}
  \item{init.angle}{initial angle}
  \item{multi}{radius scale multiplier}
  \item{col}{colors of the p2 (foreground) slices}
  \item{bg}{colors of the p1 (background) slices} 
  \item{lwd}{line width of the pie wedge boundaries} 
  \item{pie.labs}{boolean labels for the pies} 
  \item{grid}{boolean} 
  \item{grid.labs}{boolean, scale indicators} 
  \item{scale}{boolean} 
  \item{p1.circle}{boolean}
  }

\seealso{
  \code{\link{pie}}
  }

\author{Romain Francois \email{francoisromain@free.fr} & David Schruth \email{dschruth@uw.edu}}

\references{
  D. G. Feitelson (2003), "Comparing Partitions with Spie Charts".
  School of Computer Science and Engineering,
  The Hebrew University of Jerusalem.
  \url{http://www.cs.huji.ac.il/~feit/papers/Spie03TR.pdf}

  Michael Friendly (2000), Gallery of Data Vizualisation :
  The Best and Worst of Statistical Graphics.
  \url{http://www.math.yorku.ca/SCS/Gallery/}
  }

\examples{

  p1  <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
  p2<- c(0.06, 0.15, 0.52, 0.14, 0.08, 0.05)
  plot(p1, p2, multi=c(.5, 1, 1.5, 2))

}

\keyword{hplot}
