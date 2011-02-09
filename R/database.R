  dbWriteTable2 <- function(con, table.name, df, fill.null = TRUE, add.id=TRUE, ...){
    fields <- dbListFields(con, table.name)
  
    ## add id column if missing
    if(add.id){
        n <- dbGetQuery(con, paste("SELECT id FROM", table.name,"ORDER BY id DESC LIMIT 1"))[[1]]
        df$id <- 1:nrow(df) + n
    }
  
    ## look for unloadable columns in the df
    clmn.match <- match(names(df), fields)
    if(any(is.na(clmn.match)))
	warning(paste("Found '",names(df)[is.na(clmn.match)], "' not in fields of '", table.name,"' table. Omiting.", sep=''))
  
    ## ADD! section here to check for NA values in columns mapped to NOT NULL fields
  
    ## add missing fields to df
    field.match <- match(fields, names(df))
    if(fill.null == TRUE){
    	nl <- as.list(rep(NA, sum(is.na(field.match))))
    	df <- cbind(df, nl)
    	names(df) <- c(fields[clmn.match],fields[is.na(field.match)])
    } 	
    
    ## reorder df columns as per field order
    reordered.names <- names(df)[match(fields, names(df))]
    if(any(is.na(reordered.names)))
      stop('Too many unmatched columns to database column list. Stopping')
    df <- df[ ,reordered.names]
    
    ## load table
    print(paste("loading", table.name, "table to database"))
    db.write <- dbWriteTable(con, table.name, df, ...)
    
    # ADD! section in here for updating postgresql sequence
    
    if(db.write & add.id)
      return(df$id)
    else
      return(db.write)
  } 