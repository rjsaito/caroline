\name{dbWriteTable2}
\alias{dbWriteTable2}
\title{improved Wrapper for dbWriteTable.}
\description{
This wrapper for dbWriteTable 
}
\usage{
dbWriteTable2(con, table.name, df, fill.null = TRUE, add.id=TRUE, ...)
}
\arguments{
\item{con}{connection.}
\item{table.name}{The name of the table to which the data frame is to be loaded.}
\item{df}{A dataframe to be loaded to the database.}
\item{fill.null}{Should new db present fields be added to the data.frame before it is loaded?.}
\item{add.id}{Should a new column should be added for the database id?}
\item{...}{other parameters passed to dbWriteTable.}
}
\value{
If successful, the ids of the newly added database records.
}
\examples{
}
\seealso{
dbWriteTable
}

