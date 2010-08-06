## table to dataframe (with names!)
tab2df <- function(x){
  
  columns <- list()
  rows <- row.names(x)
  for(colname in colnames(x))
    columns[[colname]] <- x[,colname]
  df <- data.frame(columns)
  row.names(df) <- rows
  return(df)
  
}



## turns a named list into objects in the global environment 
delist <- function(L){
  
  names <- names(L)
  
  if(is.null(names))
    stop('list elements must have names')
  
  if(any(nchar(names)==0))
    stop('all list element names must have non zero length')
  
  for(name in names)
    assign(name, L[[name]], envir=.GlobalEnv)

}



nv <- function(x, name){

  if(class(x)=='data.frame'){
    v <- x[,name]
    names(v) <- rownames(x)
  }else{
    v <- x
    names(v) <- name
  }
  v
}
