

write.delim <- function(df, file){
	write.table(df, file,  quote=FALSE, row.names=FALSE, sep='\t')
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



.aggregateDFrow <- function(x, cols, funcs=rep('sum',length(cols))){
  if(length(cols)!= length(funcs))
    stop('cols length does not equal funcs length')
  dfl <- list()
  for(i in seq(along=cols))
    dfl[[cols[i]]] <- do.call(funcs[i], list(x[,cols[i]]))
  df <- as.data.frame(dfl);
  return(df)  
}


groupBy <- function(df, fact, cols=names(df), funcs=rep('sum',length(cols)))
  do.call(rbind.data.frame, by(df, fact, function(x) .aggregateDFrow(x, cols, funcs)))


regroupDF <- function(df, old, new, cols=names(df), funcs=rep('sum',length(cols))){
  
  if(length(old) != length(new))
    stop('old and new must be the same length')
  
  groupings <- row.names(df)
  matches <- match(old, row.names(df))
  groupings[matches[!is.na(matches)]] <- new[!is.na(matches)]

  regroup <- groupBy(df, as.factor(groupings), cols, funcs)
  return(regroup)
}





.nameLock <- function(filepath, id='')
  paste(filepath, '.LOCK', id,sep='')

.findLocks <- function(lock.filepath)
  Sys.glob(paste(lock.filepath,'*',sep=''))

.countLocks <- function(filepath)
  length(.findLocks(.nameLock(filepath)))

rmFileLock <- function(filepath, id)
  file.remove(.nameLock(filepath, id))  

getFileLock <- function(filepath, wait.time=.01, id){
  
  if(!file.exists(filepath))
    warning(paste('specified file', filepath,"doesn't exist yet"))

  while(TRUE){
    ## see if there are any existing file locks
    while(.countLocks(filepath) > 0)   #file.exists(lock.filepath))
      Sys.sleep(runif(1, 0, wait.time))
  
    ## if not, create one
    file.create(.nameLock(filepath, id))
    
    ## but wait a bit just to make sure there weren't accidently two simultaneous locks
    Sys.sleep(runif(1, wait.time/2, wait.time))
    lock.ct <- .countLocks(filepath) 
    if(lock.ct > 1)
      rmFileLock(filepath, id)
    if(lock.ct == 1)
      break()
  }    
}




.write <- function(x, file = "data", ncolumns = if (is.character(x)) 1 else 5, append = FALSE, sep = " ", lock=FALSE){
  rand.suffix <- round(runif(1,1,1500000000))
  if(lock)
    getFileLock(file, id = rand.suffix)
  cat(x, file = file, sep = c(rep.int(sep, ncolumns - 1), "\n"), append = append)
  if(lock)
    rmFileLock(file, id = rand.suffix)
}

cmdArgsToVariables <- function(){
  cmd.arg.pairs.eq <- sub("--","",commandArgs()[grep("=",commandArgs())])
  cmd.arg.pairs <- strsplit(cmd.arg.pairs.eq,"=")
  keys <- sapply(cmd.arg.pairs,"[",1)
  values <- sapply(cmd.arg.pairs,"[",2)
  ## convert the reserved logical types properly
  rsvd <- list('TRUE', 'FALSE', 'NA','NULL')  #NULL gets converted to NA
  if(length(keys) > 0 & length(values) > 0){ 	
    for(i in seq(along=values)){
      value <- values[i]
      for(rsv in rsvd){
      	if (values[i] == tolower(rsv) | values[i] == rsv)
          value <- as.logical(rsv)
      }
      assign(keys[i],value,envir=.GlobalEnv)      
    }
  }else{
    warning("Couldn't find any command line arguments. No variables to assign.")
  }
}


parseArgString <- function(string, delimiter=',', min.param.ct=2, max.param.ct=2, param.range=NULL){
## generic function for parsing delimited lists from BATCH mode argument strings.

  # count the delimiters
  re <- regexpr(delimiter, string)[[1]]
  delim.ct <- length(re)
  if(delim.ct >= max.param.ct)
    stop(paste('parameter count', delim.ct+1,'is too high for argument', string))
  if(re == -1 & min.param.ct != 1)
    stop(paste('you need a',delimiter,'delimited dimentions for this argument', string))

  p.vect <- strsplit(string, delimiter)[[1]]

  # check the range
  if(!is.null(param.range)){
    if(any(class(param.range[1]) == 'POSIXct'))    #if(class(param.range) == 'Date')
      p.vect <- as.POSIXct(p.vect)  #as.Date
    else
      p.vect <- as(p.vect, class(param.range))
    if(class(param.range) == 'factor')
      param.range <- as.character(param.range)
    if(class(param.range) == 'character'){
      if(!all(p.vect %in% param.range))
        stop(paste('not all of the elements of your parameter list', string, 'could be found in the allowed possiblities list', paste(param.range,collapse=',')))
    }else{
      if(!(param.range[1] <= range(p.vect)[1] & range(p.vect)[2] <= param.range[2]))
        stop(paste('the range you passed',string,'exceeds the allowed limits',paste(param.range,collapse=',')))
    }
  }
  return(p.vect)
}


.createBatchCommand <- function(cmdopts, script, logfile=paste(script,'.out',sep='')){
  cmd.rcmdbatch <- "R CMD BATCH --no-save --no-restore "
  cmd <- paste(cmd.rcmdbatch, cmdopts, script , logfile)
  return(cmd)
}
