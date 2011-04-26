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
segments
}
\examples{

}

