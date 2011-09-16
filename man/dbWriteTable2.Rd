\name{dbWriteTable2}
\alias{dbWriteTable2}
\title{Data Import Wrapper for dbWriteTable.}
\description{
This is wrapper for dbWriteTable written with the the primary improvements focusing on database import into an existing table definition schema.  
The function matches and rearranges columns of the dataframe to database feilds and additionally performs checks for NA's in required variables, overlength strings, and type mismatches.
There also exists support for updating of the PostgreSQL specific sequence for tables with auto incrementing primary keys.
}
\usage{
dbWriteTable2(con, table.name, df, fill.null = TRUE, add.id=TRUE, row.names=FALSE, pg.update.seq=FALSE, ...)
}
\arguments{
\item{con}{connection.}
\item{table.name}{The name of the table to which the data frame is to be loaded.}
\item{df}{A dataframe to be loaded to the database.}
\item{fill.null}{Should new db present fields be added to the data.frame before it is loaded?.}
\item{add.id}{Should a new column should be added for the database id?}
\item{row.names}{Should the row names be loaded as a seperate column? (unlike the original dbWriteTable, default is FALSE)}
\item{pg.update.seq}{should the table primary key's sequence be updated to the highest id value +1? (Postgres specific)}
\item{...}{other parameters passed to dbWriteTable.}
}
\value{
If successful, the ids of the newly added database records (invisible)
}
\examples{
}
\seealso{
\link[DBI:dbReadTable]{dbWriteTable}
}

	