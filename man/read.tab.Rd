\name{read.tab}
\alias{read.tab}
\title{Read in a Tab Delimited File.}
\description{
This function is a slight (genome annotation friendly) variant of the built-in read.delim function in R.  
Two non-standard defaults have been set: stringsAsFactors=TRUE, quote="". 
An additional parameter "check.row.ct", triggering a count.fields call, has been added to further ensure the integrity of large data files.
}
\usage{
read.tab(file, check.row.ct = TRUE, stringsAsFactors = FALSE, 
               quote = "", header=TRUE, ...)
}
\arguments{
\item{file}{the name of the file which the data are to be read from.}
\item{check.row.ct}{logical: use 'count.fields' to independently verify the number of rows read.table reads into memory?}
\item{stringsAsFactors}{logical: should character vectors be converted to factors?.}
\item{quote}{the set of quoting characters.}
\item{header}{boolean specifying if the first row serves as labels for the columns}
\item{...}{other paramters passed to read.delim.}
}
\value{
a dataframe.
}
