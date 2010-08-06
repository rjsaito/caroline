
## FILE I/O

write.delim <- function(df, file){
	write.table(df, file,  quote=FALSE, row.names=FALSE, sep='\t')
}


## SIMULTANEOUS FILE WRITING ##

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





