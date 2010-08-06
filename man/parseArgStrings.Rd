\name{parseArgString}
\alias{parseArgString}
\title{Process Command Line Arguments}
\description{
generic function for parsing delimited lists from BATCH mode argument strings.
}
\usage{
parseArgString(string, delimiter=',', min.param.ct=2, max.param.ct=2, param.range=NULL)
}
\arguments{
\item{string}{string to parse.}
\item{delimiter}{how the string is delmited into a vector.}
\item{min.param.ct}{minimum number of parameters in the vector.}
\item{max.param.ct}{maximum number of parameters in the vector.}
\item{param.range}{the range of the parameter values.}
}
\value{
a vector or value that has been check for validity
}
\examples{

## passes
parseArgString('apple,banana,pear', param.range=c("apple","banana","pear","pineapple"))
parseArgString('1,2,3', param.range=c(1,4))


## fails
\dontrun{
parseArgString('apple,banana,pear', param.range=c("apple","banana"))
parseArgString('1,2,3', param.range=c(1,2))
}

}
\keyword{manip}

