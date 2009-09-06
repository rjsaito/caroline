                                          
truncPath <- function(path, limit=256, minlen=15){
    ncharpath <- nchar(path)
    if(ncharpath > limit){
        print (paste("Path name length of",ncharpath,"is too long."))
        filename <- basename(path)
        directory <- paste(dirname(path),"/",sep="")
        nchardir <- nchar(directory)
        ncharfile <- nchar(filename)
        ncharfile.new <- ncharfile - (ncharpath - limit)
        ncharstart <- ncharfile - ncharfile.new
        if(nchardir < (limit - minlen)){
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

pad <- function(vect,np){
 x <- as.numeric(vect)
 vect <- as.character(vect)
 for(p in 10^(1:np)){
   vect[x<p] <- paste("0",vect[x<p],sep="")
 }
 vect
}


createVariablesFromCommandArgs <- function(){
  cmdArgPairsEq <- sub("--","",commandArgs()[grep("=",commandArgs())])
  cmdArgPairs <- strsplit(cmdArgPairsEq,"=")
  keys <- sapply(cmdArgPairs,"[",1)
  values <- sapply(cmdArgPairs,"[",2)
  for(i in 1:length(keys)){
    assign(keys[i],values[i],envir=.GlobalEnv)
  }
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



makeElipseCoords <- function(x0 = 0, y0 = 0, b = 1, a = 1, alpha = 0, pctRange = c(0,1), len = 50){
  radRange <- 2 * pi * pctRange
  theta <- seq(radRange[1], radRange[2], length=(len))
  x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
  y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)
  cbind(x,y)
}


plotClock <- function(hour, minute, title, x0 = 0, y0 = 0, r = 1){  #ampm = "notUsed"
  
  circleXY <- makeElipseCoords(x0 = x0, y0 = y0, b = 1.1*r, a = 1.1*r, alpha = 0, pctRange = c(0,1), len = 50)
  quarHourTickMarksXY <- makeElipseCoords(x0 = x0, y0 = y0, b = 1.05*r, a = 1.05*r, alpha = (pi/2), pctRange = c((12*4-1)/(12*4),0), len = 12*4)
  hourLabelsXY <- makeElipseCoords(x0 = x0, y0 = y0, b = .9*r, a = .9*r, alpha = (pi/2), pctRange = c(11/12,0), len = 12)

  polygon(circleXY)
  text(hourLabelsXY[,1],hourLabelsXY[,2],seq(1,12), cex=.5)
  text(quarHourTickMarksXY[,1],quarHourTickMarksXY[,2],".")

  minuteV <- minute/12
  minuteVXY <- makeElipseCoords(x0 = x0, y0 = y0, b = r, a = r, alpha = 0, pctRange =  (.25 - c(minuteV,minuteV)), len = 1)
  segments(x0,y0,minuteVXY[1],minuteVXY[2])

  hourV <- hour/12
  hourVXY <- makeElipseCoords(x0 = x0, y0 = y0, b = .7*r, a =.7*r, alpha = 0, pctRange = (.25 - c(hourV,hourV)), len = 1)
  segments(x0,y0,hourVXY[1],hourVXY[2])  

}

