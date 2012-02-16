\name{wjitter}
\alias{wjitter}
\title{Weighted Jitter}
\description{
Use weights to jitter values away fromt their current value.
}
\usage{
wjitter(x, w, amount=.43)
}
\arguments{
\item{x}{a vector of values}
\item{w}{a vector of weights of the same lenght as x}
\item{amount}{the amount to jitter (passed to the parameter by the same name in the jitter function)}
}
\value{
A weighted jittered vector of the same length as x
}
\examples{
x <- seq(1,20)
w <- runif(20, 0,1)
plot(x,wjitter(w,x))
}
