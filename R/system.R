
cmdArgsToVariables <- function(){
  cmd.arg.pairs.eq <- sub("--","",commandArgs()[grep("=",commandArgs())])
  cmd.arg.pairs <- strsplit(cmd.arg.pairs.eq,"=")
  keys <- sapply(cmd.arg.pairs,"[",1)
  values <- sapply(cmd.arg.pairs,"[",2)
  if(length(keys) > 0 & length(values) > 0){ 	
    for(i in seq(along=values))
      assign(keys[i],values[i],envir=.GlobalEnv)      
  }else{
    print("WARNING: Couldn't find any command line arguments. No variables to assign.")
  }
}


tab2df <- function(x){
  
  columns <- list()
  rows <- row.names(x)
  for(colname in colnames(x))
    columns[[colname]] <- x[,colname]
  df <- data.frame(columns)
  row.names(df) <- rows
  return(df)
  
}
