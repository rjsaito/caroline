\name{m}
\alias{m}
\title{Regexp Match Operator}
\description{
A grep/sub-like function that returns one or more back-referenced pattern matches in the form of a vector or as columns in a dataframe (respectively).  
Unlike sub, this function is more geared towards data extraction rather than data cleaning. 
The name is derived from the popular PERL regular expression 'match' operator function 'm' (eg. 'extraction =~ m/sought_text/'). 
}
\usage{
m(pattern, vect, names="V", types="character", mismatch=NA, ...)
}
\arguments{
\item{pattern}{A regular expression pattern with at least one back reference.}
\item{vect}{A string or vector of strings one which to apply the pattern match.}
\item{names}{The vector of names of the new variables to be created out of vect. Must be the same length as vect.}
\item{types}{The vector of types of the new variables to be created out of vect. Must be the same length as vect.}
\item{mismatch}{What do to when no pattern is found.  NA returns NA, TRUE returns original value (currently only implimented for single match, vector returns)}
\item{...}{other parameters passed on to grep}
}
\seealso{
\code{\link{sub}, \link{gsub}, \link{regexpr}, \link{grep}, \link{gregexpr}}.
}
\value{
Either a vector or a dataframe depending on the number of backreferences in the pattern.
}
\examples{

## single vector output examples
m(pattern="asdf.([A-Z]{4}).", 
  vect=c('asdf.AS.fds','asdf.ABCD.asdf', '12.ASDF.asdf','asdf.REWQ.123'))


Rurls <- c('http://www.r-project.org',    'http://cran.r-project.org',
           'http://journal.r-project.org','http://developer.r-project.org')
m(pattern="http://([a-z]+).r-project.org", vect=Rurls)


# dataframe output examples

data(mtcars)
m(pattern="^([A-Za-z]+) ?(.*)$", 
  vect=rownames(mtcars), names=c('make','model'), types=rep('character',2))


}
\keyword{manip}

