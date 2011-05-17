\name{raPlot}
\alias{raPlot}
\title{Generate a Ratio Average [RAy] Plot.}
\description{
A plot which turns two vectors of count data into log scaled fold change ratio and average abundance.  
The plot derives from a Bland-Altman plot and is also very similar to an MA plot.  
The RA plot is unique, however, in it's creative inclusion of the vector-unique 'arms' which are artificially introduced into the plot by adding a <1 epsilon factor before the log function is applied.
The name RAy comes from the fact that the aforementioned 'uniques' arms addition makes it strongly resemble a geometric ray.  
Many of the parameters to the function play off of this convenient anatomical analogy.
The plot contains it's own built in significant lines (drawn using the binomial distribution) and displayed via the 'tail' parameter.
}
\usage{
raPlot(a, b=NULL, uniques=5, normalize=FALSE,  
                   nr=0, alpha = 0.01, jitter=FALSE, jit.wgts=NULL,
                   rex=1, flat=TRUE, tail=.5, arms=.5, spine=1, border=NULL, ...)
}
\arguments{
\item{a}{a vector of counts for a.  can also be a matrix with two columns 1 for a and 2 for b.}
\item{b}{a vector of counts for b.}
\item{uniques}{a boolean specifying whether or not to plot the library-unique genes (those with zero counts in one or the other library).}
\item{normalize}{A boolean specifying whether or not to normalize the counts into proportions.}
\item{nr}{a numeric value indicating the asymptotic normalization ratio line.}
\item{alpha}{a statisical significance value.}
\item{jitter}{whether or not or how much to jitter the a and b counts into surrounding, non-overlapping space.}
\item{jit.wgts}{a weight vector used to spread the counts of a and b into surrounding, non-overlapping space.}
\item{rex}{a numeric value specifying the radial expansion of the plot points.}
\item{flat}{a boolean for the radial expansion of points as a function of both R and A axes.}
\item{tail}{a numeric or boolean value indicating the line thickness of the two trailing curved significance lines of the RAy.}
\item{arms}{a numeric or boolean value indicating the line thickness of the two leading straight separator lines of the RAy.}
\item{spine}{a numeric or boolean value indicating the line thickness of the normalization line (whose y position is specified by mm).}
\item{border}{a vector of strings used to color the borders of the points.}
\item{...}{other parameters passed to plot.}
}
\value{
a RAy plot
}
\seealso{
manta.ra, plotMA, maPlot
}
\examples{



}

