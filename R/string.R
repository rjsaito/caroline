

pad <- function(vect,np){
 x <- as.numeric(vect)
 vect <- as.character(vect)
 for(p in 10^(1:np)){
   vect[x<p] <- paste("0",vect[x<p],sep="")
 }
 vect
}



m <- function(pattern, vect, names="V", types="character"){
  matches <- regexpr(pattern , vect )
  n <- length(gregexpr("[^\\]?)",pattern)[[1]]) #how many groups (non literal perends) does this pattern have
  if(n != length(names) | n != length(types)){
    print("ERROR: the number of backreferences in 'pattern' must equal the length of the 'names' and 'types' vectors")
  }else{
    ret <- list() 
    for(i in 1:n){
      this.vect <- as.vector(rep(NA,length(vect), mode=types[i]))
      this.vect[matches > 0] <- as.vector(sub(paste("(.*)", pattern ,"(.*)",sep=""),paste("\\",i+1,sep=""), vect[matches > 0]), mode=types[i]) 
      ret[[names[i]]] <- this.vect
    }
    if(n == 1)
      as.vector(ret[[1]])
    else
      as.data.frame(ret)
  }
}


truncPath <- function(path, limit=256, min.length=15){
    n.char.path <- nchar(path)
    if(n.char.path > limit){
        print (paste("Path name length of",n.char.path,"is too long."))
        filename <- basename(path)
        directory <- paste(dirname(path),"/",sep="")
        nchardir <- nchar(directory)
        ncharfile <- nchar(filename)
        ncharfile.new <- ncharfile - (n.char.path - limit)
        ncharstart <- ncharfile - ncharfile.new
        if(nchardir < (limit - min.length)){
            truncd.path <- paste(directory, substr(filename, ncharstart ,ncharfile), sep='')
            print (paste("truncating first", ncharstart ,"characters of the file name"))
            return(truncd.path )
        }else{
            print (paste("Directory name length of",nchardir,"exceedes limit. Can't truncate path: \n",path))
            return(NULL)
        }
    }else{
        return(path)
    }
}


