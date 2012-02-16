
## FILE I/O

write.delim <- function(df, file, quote=FALSE, row.names=FALSE, sep='\t', ...){
  write.table(df, file,  quote=quote, row.names=row.names, sep=sep, ...)
}

#write.tab <- function(df, file, quote=FALSE, row.names=TRUE, sep='\t', ...){
#  write.table(df, file,  quote=quote, row.names=row.names, sep=sep, ...)
#}


read.tab <- function(file, check.row.ct=TRUE, stringsAsFactors=FALSE, quote="", ...){
  out <- read.delim(file, quote=quote, stringsAsFactors=stringsAsFactors, ...)
  if(check.row.ct){
    line.ct <- length(count.fields(file)) - as.integer(header) 
    if(nrow(out) != line.ct)
      stop(paste("Number of rows in dataframe (",nrow(out),") doesn't equal (non-header) line count (", line.ct,")",sep=''))
  }
  return(out)
}
