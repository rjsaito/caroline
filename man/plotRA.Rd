\name{plotRA}
\alias{plotRA}
\title{Ratio Average Plot}
\description{
A plot for displaying logarythmic binary treatment effect on count data.  
Although groups of positive integers are required as input, 
a weights vector can be supplied to create a continuous output. 
Very similar to an microarray "MA plot" or a "Tukey Mean-distance plot". 
Confidence intervals (generated from binomial distribution draws) 
as well as the left hand asymptotic boundaries can optionally be added to the plot.
This function also supports the addition of subgroup pie charts instead of points. 
}
\usage{
plotRA(x, subgroups=NULL, weights=NULL, prob=0.5, sig.lines=TRUE, asymp.lines=TRUE, epsilon=NULL, plot=TRUE)
}
\arguments{
\item{x}{dataframe that includes at a binary factor called 'treatment' and another called 'group' and an optional two factors as follows}
\item{subgroups}{a list of sub group factors split on and named by group. used in pie chart points.}
\item{weights}{a vector of weights. must average out to ~1.  multiplied by the tabulation counts.}
\item{prob}{probability that draw will occur treatment 'a' vs 'b' (fairness of coin flip).}
\item{sig.lines}{a boolean to specify if the significance lines should be drawn.}
\item{asymp.lines}{a boolean to specify if the asympotitc left hand boundary should be drawn.}
\item{epsilon}{the fudge factor to allow non bi-treatment represented into the log(a/b) plot.}
\item{plot}{a boolean to specify if data should be ploted or just dumped as a cross tabulation.}
}
\seealso{
plotMA, tmd
}
\examples{

n <- 20000
n.groups <- 1000
n.subgroups <-  6

treats <- c('plus','control')[round(runif(n,1,2))]
grps <- paste('gene',seq(1,n.groups), sep='')[round(runif(n,1,n.groups))]
subgrps <- paste('species',seq(1,n.subgroups), sep='')[round(runif(n,1,n.subgroups))]
x <- data.frame(treatment=treats,group=grps, subgroup=subgrps, weight=runif(length(grps),0,2))
plotRA(x)

}
\keyword{}

