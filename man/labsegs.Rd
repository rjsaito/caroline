\name{labsegs}
\alias{labsegs}
\title{Buffered Segments for Point Labels}
\description{
This function is a wrapper for segments which trigonometrically shortens the lines that are near the "1" end so as not to cluter or overplot the text label it is attached to.
}
\usage{
labsegs(x0, y0, x1, y1, buf=.3, ...)
}
\arguments{
\item{x0}{initial x point coordinate}
\item{y0}{initial x point coordinate}
\item{x1}{initial x point coordinate}
\item{y1}{initial x point coordinate}
\item{buf}{the buffer between the label at point "1" and the actual segment}
\item{...}{other parameters passed to segments.}
}
\seealso{
\link{segments}
}
\examples{

x <- rnorm(1000,0,.5)
y <- rnorm(1000,-.3,.15)

labdb <- data.frame(x=seq(-.5,.5,by=.5), y=rep(.85,3))
xlims <- c(-1,1)
ylims <- c(-.5,1)

x0.lbd <- x[rev(order(y))][1:3]
y0.lbd <- y[rev(order(y))][1:3]

par(mfrow=c(1,2))
plot(x,y, xlim=xlims, ylim=ylims, main='segments')
segments(x0=x0.lbd, y0=y0.lbd, x1=labdb$x, y1=labdb$y, col=rainbow(3), lwd=3)
text(x=labdb$x, y=labdb$y, labels=letters[1:3], cex=3, col=rainbow(3))

plot(x,y, xlim=xlims, ylim=ylims, main='labsegs')
labsegs(x0=x0.lbd, y0=y0.lbd, x1=labdb$x, y1=labdb$y, col=rainbow(3), lwd=3, buf=.07)
text(x=labdb$x, y=labdb$y, labels=letters[1:3], cex=3, col=rainbow(3))


}

