\name{pies}
\alias{pies}
\title{Pie chart scatterplot}
\description{
Plot pie charts in an XY scatterplot. An overhaul of the original pie plot function.
}
\usage{
pies(x, show.labels = FALSE, show.slice.labels = FALSE, color.table = NULL, 
		radii = 0.2, x0=NULL, y0=NULL, xlim=c(-1,1), ylim=c(-1,1),
		edges = 200,  clockwise = FALSE, 
                init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
                border = NULL, lty = NULL, main = NULL) 
}
\arguments{
\item{x}{a list of named vectors.}
\item{show.labels}{boolean specifying if the pie point lables should be plotted.}
\item{show.slice.labels}{boolean specifying if the pie slice labels should be plotted.}
\item{color.table}{a named vector of colors. names should correspond to all possible levels of x}
\item{radii}{a vector of radii used to size the pie points.}
\item{x0,y0}{a vector of x and y positions for the pie points.}
\item{xlim, ylim}{the x and ylimits for the plot.}
\item{edges}{the circular outline of the pie is approximated by a polygon with this many edges.}
\item{clockwise}{logical indicating if slices are drawn clockwise or counter clockwise (i.e., mathematically positive direction), the latter is default.}
\item{init.angle}{number specifying the starting angle (in degrees) for the slices. Defaults to 0 (i.e., 3 o'clock) unless clockwise is true where init.angle defaults to 90 (degrees), (i.e., 12 o'clock).}
\item{density}{the density of shading lines, in lines per inch. The default value of NULL means that no shading lines are drawn. Non-positive values of density also inhibit the drawing of shading lines.}
\item{angle}{the slope of shading lines, given as an angle in degrees (counter-clockwise).}
\item{border}{(possibly vectors) arguments passed to polygon which draws each slice.}
\item{lty}{(possibly vectors) arguments passed to polygon which draws each slice.}
\item{main}{an overall title for the plot.}
}
\seealso{
pie
}
\examples{
n <- 200
n.groups <- 10
n.subgroups <-  6

grps <- paste('gene',seq(1,n.groups), sep='')[round(runif(n,1,n.groups))]
subgrps <- paste('species',seq(1,n.subgroups), sep='')[round(runif(n,1,n.subgroups))]
group.df <- cbind.data.frame(grps,subgrps)
subgroup.list <- by(group.df, group.df$grps, function(x) x$subgrps)

pie.list <- lapply(subgroup.list, table)
pies(x=pie.list, x0=rnorm(n.groups), y0=rnorm(n.groups), show.labels=TRUE, show.slice.labels=TRUE)
}
\keyword{}

