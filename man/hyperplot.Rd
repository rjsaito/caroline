\name{hyperplot}
\alias{hyperplot}
\title{Annotate Outliers in a Scatterplot via an HTML Image-Map}
\description{
This simple function makes R scatter plots interactive by creating an image and wrapping HTML around it: creating a hyperlinked hyperplot.  
Hover over the points to see what each is. 
Click to connect to a table below that will tell you more about each point (if browse ==TRUE).
}
\usage{
hyperplot(x, y=NULL, annout, name='hyperplot.image_map', w=72*8, h=72*6, link2tab=TRUE, browse=TRUE, cex=1, ...)
}
\arguments{
\item{x}{a plot-able object, a numeric vector or the name of a numeric vector in annout.}
\item{y}{a numeric vector or the name of a numeric vector in annout. Must be the same length as x.}
\item{annout}{a named data.frame or table of outliers to annotate the points in the plot. 'x' and 'y' params can indicate column names or numbers of annout, but 'out' boolean column should exist as well.}
\item{name}{base name of the image & html (map) page that get generated.}
\item{w}{width of the png image in inches.}
\item{h}{height of the png image in inches.}
\item{link2tab}{create a linked lookup table from image to the annout table.}
\item{browse}{load the html page automatically via R.}
\item{cex}{character expansion for points}
\item{...}{other paramters passed on to plot()}
}
\seealso{
imagemap(imagemap),imageMap(Rgraphviz),\link{browseURL}
}
\value{HTML page with annotation mapped image}
\examples{

if(capabilities()["png"] && interactive()) {


## x and y as numeric vectors
x <- nv(rnorm(30),1:30)
y <- rnorm(30)
hyperplot(x,y, annout=1:15)

## annout as dataframe (x and y as vectors)
df <- data.frame(out=rep(c(TRUE,FALSE),15),row.names=as.character(1:30))
hyperplot(x,y, annout=df)


## x and y as names of columns in df
df <- data.frame(x=x, y=y, out=rep(c(TRUE,FALSE),15),row.names=as.character(1:30))
hyperplot(x='x',y='y', annout=df)


}

}

