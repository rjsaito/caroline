\name{pies}
\alias{pies}
\title{Pie chart scatterplot}
\description{
Plot pie charts in an XY scatterplot. An overhauled wrapper of the original pie plot function.  It is currently very slow: a recommened work around is to plot to something other than the default device (aka png, pdf, etc).
}
\usage{
pies(x, show.labels = FALSE, show.slice.labels = FALSE, color.table = NULL, 
		radii = rep(2,length(x)), x0=NULL, y0=NULL, 
		edges = 200,  clockwise = FALSE, 
                init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
                border = NULL, lty = NULL, 
                other.color='gray', na.color='white', ...)
}
\arguments{
\item{x}{a list of named vectors.}
\item{show.labels}{boolean specifying if the pie point lables should be plotted.}
\item{show.slice.labels}{boolean specifying if the pie slice labels should be plotted.}
\item{color.table}{a named vector of colors. names should correspond to all possible levels of x}
\item{radii}{a vector of radii used to size the pie points.}
\item{x0,y0}{a vector of x and y positions for the pie points.}
\item{edges}{the circular outline of the pie is approximated by a polygon with this many edges.}
\item{clockwise}{logical indicating if slices are drawn clockwise or counter clockwise (i.e., mathematically positive direction), the latter is default.}
\item{init.angle}{number specifying the starting angle (in degrees) for the slices. Defaults to 0 (i.e., 3 o'clock) unless clockwise is true where init.angle defaults to 90 (degrees), (i.e., 12 o'clock).}
\item{density}{the density of shading lines, in lines per inch. The default value of NULL means that no shading lines are drawn. Non-positive values of density also inhibit the drawing of shading lines.}
\item{angle}{the slope of shading lines, given as an angle in degrees (counter-clockwise).}
\item{border}{(possibly vectors) arguments passed to polygon which draws each slice.}
\item{lty}{(possibly vectors) arguments passed to polygon which draws each slice.}
\item{other.color}{color used for x vector elements for names without corresponding names in the color table}
\item{na.color}{color used for x vector elements with missing names}
\item{...}{other arguments passed to polygon}
}
\seealso{
\link{pie}
}
\value{
Pie charts as points on a plot
}
\examples{

## these examples are to the default plot window, which can be slow
## try instead to plot to png or pdf for example

## example 1
  pies(
       list(
            a=nv(c(1,2,3),c('one','two','thre')),
            b=nv(c(2,2,3),c('one','two','thre')),
            c=nv(c(1,2,3),c('one','two','thre'))
            ),
       x0=c(0,.5,1),
       y0=c(0,.5,1), radii=6, border=c('gray', 'black', 'red')
       )


## example 2
n <- 200
n.groups <- 10
n.subgroups <-  6

grps <- paste('gene',seq(1,n.groups), sep='')[round(runif(n,1,n.groups))]
subgrps <- paste('species',seq(1,n.subgroups), sep='')[round(runif(n,1,n.subgroups))]
group.df <- cbind.data.frame(grps,subgrps)
subgroup.list <- by(group.df, group.df$grps, function(x) x$subgrps)

pie.list <- lapply(subgroup.list, table)
col.tab <- nv(rainbow(6), unique(subgrps))

pies(x=pie.list, x0=rnorm(n.groups), y0=rnorm(n.groups), radii=10, show.labels=TRUE, show.slice.labels=TRUE, color.table=col.tab)


## example 3  reading from external flat file
## salt.df <- read.delim('/path/to/my/file.tab')
## create a dummy dataset that might live inside the above file
salt.df <- data.frame(salinity=rnorm(25,5), temperature=rnorm(25,25), spec_a=rpois(25,4), 
							   spec_b=rpois(25,4), 
							   spec_c=rpois(25,4), 
							   spec_d=rpois(25,4),
							   spec_e=rpois(25,4)
							   )
## pull out the colnumn names that are specific to pie wedge numbers	
salt.spec.nms <- names(salt.df)[grep('spec',names(salt.df))]
## turn them into a list
pie.list <- lapply(1:nrow(salt.df), function(i) as.table(nv(as.vector(as.matrix(salt.df[i,salt.spec.nms])), salt.spec.nms)))
names(pie.list)<- letters[1:25]
with(salt.df, pies(x=pie.list, x0=salinity, y0=temperature, radii=2))

}