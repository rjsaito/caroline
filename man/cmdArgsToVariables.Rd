\name{cmdArgsToVariables}
\alias{cmdArgsToVariables}
\title{Create R variables from command line parameters}
\description{
Take the names of the variables specified on the R CMD BATCH call and turn them into R variables
Assign the values after '=' to these variables.
}
\usage{
cmdArgsToVariables()
}

\value{
variable(s) stored in memory
}
\examples{

\dontrun{
## R CMD BATCH --some.variable=testing
cmdArgsToVariables() 
print(some.variable)
#> 'testing'
}
}
\keyword{IO}

