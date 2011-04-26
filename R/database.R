  dbWriteTable2 <- function(con, table.name, df, fill.null = TRUE, add.id=TRUE, ...){
    fields <- dbListFields(con, table.name)
  
    ## add id column if missing
    if(add.id){
      last.id.list <- dbGetQuery(con, paste("SELECT id FROM", table.name,"ORDER BY id DESC LIMIT 1"))
      if(length(last.id.list)==0)
        n <- 0
      else
        n <- last.id.list[[1]]
      df$id <- 1:nrow(df) + n
    }
  
    ## look for unloadable columns in the df
    clmn.match <- match(names(df), fields)
    if(any(is.na(clmn.match)))
	warning(paste("Found '",names(df)[is.na(clmn.match)], "' not in fields of '", table.name,"' table. Omiting.\n", sep=''))
  
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

    
    ## check for na's which might prevent a load
    r <- dbSendQuery(con, paste("SELECT * FROM", table.name,"ORDER BY id DESC LIMIT 1"))
    null.OK <- nv(dbColumnInfo(r)$nullOK, dbColumnInfo(r)$name)
    dummy <- fetch(r)
    reqd.fields <- names(null.OK[!null.OK])
    na.cols <- sapply(df, function(x) any(is.na(x)) )
    req.miss <- na.cols[reqd.fields]
    if(any(req.miss))
      stop(paste("Didn't load your dataframe because required field(s)", names(req.miss)[req.miss],"contained missing values"))

    
    ## load table
    print(paste("loading", table.name, "table to database"))
    db.write <- dbWriteTable(con, table.name, df, ...)
    
    # ADD! section in here for updating postgresql sequence
    
    if(db.write & add.id)
      return(df$id)
    else
      return(db.write)
  } 
