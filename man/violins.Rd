\name{volins}
\alias{violins}
\author{Daniel Adler <dadler@uni-goettingen.de>,
	 David Schruth <dschruth@u.washington.edu>,
	 Dave McArthur <dmca@ucla.edu>}
\title{Plot a Series of Vioplot Violins}
\description{
This is an improvment on the original vioplot function (combination of a box plot and a kernel density plot).  You can now pass a list of violins and a list of colors. Optionally it also returns statistics on the distributions as well.
}
\usage{
violins(x, by, range = 1.5, h = NULL, ylim = NULL, names =
                 NULL, horizontal = FALSE, col = "transparent", border
                 = "black", lty = 1, lwd = 1, rectCol = "grey50",
                 colMed = "grey80", pchMed = 19, at, add = FALSE, wex =
                 1, drawRect = TRUE, main = "", xlab = "", ylab = "",
                 connect = c("median", "mean", "hubermu", "deciles"),
                 SD.or.SE = c("SD"), connectcol = c("lightblue",
                 "cyan", "darkred", "grey"), las = 2, stats = FALSE,
                 quantiles = c(0.1, 0.9), CImed = TRUE, deciles = TRUE)

}
\arguments{
\item{x}{list of vectors or a dataframe}
\item{by}{accepts 'by' object for grouping}
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
\item{connect}{connects a violin plot series at the medians with line segments}
\item{connectcol}{line color of the median connecting line segments}
\item{xlab}{x axis label}
\item{ylab}{y axis label}
\item{stats}{print out statistics}
\item{SD.or.SE}{Boolean for standard deviation or standard error}
\item{las}{axis tick mark labels orientation}
\item{quantiles}{map any pair of quantiles (as dotted box) in addition to Q1 & Q3, but are not shown when "c(0,0)" and arg is passed to descriptive stats when 'stats'=TRUE}
\item{CImed}{portrays 95 percent confidence intervals for the median (as solid box)}
\item{deciles}{maps deciles 0.1:0.9 (as thin lines) independently of 'quantiles' and can be connected when 2 or more variables are plotted.}
}
\seealso{
vioplot(vioplot), \link[sm]{sm.density}
}
\value{a series of violin plots}
\examples{

n <- rnorm(130, 10, 3)
p <- rpois(110, 4)
u <- runif(300, 0, 20)
l <- rlnorm(130, log(2))
g <- rgamma(140, 3)
e <- rexp(160)

violins(list(e=e, p=p,u=u,n=n,l=l,g=g), ylim=c(0,20),
         col=c('purple','lightblue','lightgreen','red','orange','yellow'),
         stats=TRUE)
}
